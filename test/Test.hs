module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.IORef (IORef, newIORef)
import Data.Text qualified as Text
import Hoogle (Database, Target(..), defaultDatabaseLocation, withDatabase, hoogle)
import McpHoogle.Format (formatTarget, formatTargets, stripHtmlTags)
import McpHoogle.Tools (HoogleTool(..), SearchParams(..), SearchTypeParams(..), LookupModuleParams(..), handleTool)
import System.Directory (doesFileExist, createDirectoryIfMissing, getTemporaryDirectory)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "McpHoogle"
  [ formatTests
  , stripHtmlTests
  , withResource acquireDb releaseDb hoogleSearchTests
  ]

-- | Acquire a hoogle database for testing.
-- Uses the default location if it exists, otherwise generates a fresh one.
acquireDb :: IO FilePath
acquireDb = do
  defaultPath <- defaultDatabaseLocation
  dbExists <- doesFileExist defaultPath
  if dbExists
    then pure defaultPath
    else do
      tmpDir <- getTemporaryDirectory
      let testDbDir = tmpDir <> "/mcp-hoogle-test"
          testDbPath = testDbDir <> "/test.hoo"
      createDirectoryIfMissing True testDbDir
      hoogle ["generate", "--local", "--database=" <> testDbPath]
      pure testDbPath

-- | No-op cleanup (temp files are fine to leave)
releaseDb :: FilePath -> IO ()
releaseDb _ = pure ()

formatTests :: TestTree
formatTests = testGroup "Format"
  [ testCase "formatTargets empty returns no results message" $
      formatTargets [] @?= "No results found."
  , testCase "formatTarget includes package name" $ do
      let target = mkTarget "map" (Just ("containers", "")) (Just ("Data.Map", ""))
          result = formatTarget target
      assertBool "should contain package name" (Text.isInfixOf "containers" result)
  , testCase "formatTarget includes module name" $ do
      let target = mkTarget "map" (Just ("containers", "")) (Just ("Data.Map", ""))
          result = formatTarget target
      assertBool "should contain module name" (Text.isInfixOf "Data.Map" result)
  , testCase "formatTarget includes function name" $ do
      let target = mkTarget "<s0>map</s0> :: (a -> b) -> [a] -> [b]" Nothing Nothing
          result = formatTarget target
      assertBool "should contain function name after stripping html" (Text.isInfixOf "map" result)
  ]

stripHtmlTests :: TestTree
stripHtmlTests = testGroup "stripHtmlTags"
  [ testCase "strips simple tags" $
      stripHtmlTags "<b>hello</b>" @?= "hello"
  , testCase "strips span tags from hoogle output" $
      stripHtmlTags "<span class=name><s0>map</s0></span> :: (a -> b) -> [a] -> [b]"
        @?= "map :: (a -> b) -> [a] -> [b]"
  , testCase "preserves plain text" $
      stripHtmlTags "no tags here" @?= "no tags here"
  , testCase "handles nested tags" $
      stripHtmlTags "<a><b>deep</b></a>" @?= "deep"
  ]

-- | Integration tests that exercise hoogle search through our handler.
-- The database path is shared across all tests via withResource.
hoogleSearchTests :: IO FilePath -> TestTree
hoogleSearchTests getDbPath = testGroup "Hoogle search integration"
  [ testCase "search for 'map' returns results" $ do
      dbPath <- getDbPath
      withHoogleDb dbPath $ \databaseRef -> do
        result <- handleTool databaseRef (Search (SearchParams "map"))
        assertBool "search should return results, not 'No results found'"
          (result /= "No results found.")
        assertBool "search for map should mention 'map' in results"
          (Text.isInfixOf "map" result)
  , testCase "type search '[a] -> Int' returns results" $ do
      dbPath <- getDbPath
      withHoogleDb dbPath $ \databaseRef -> do
        result <- handleTool databaseRef (SearchType (SearchTypeParams "[a] -> Int"))
        assertBool "type search should return results"
          (result /= "No results found.")
  , testCase "module lookup 'Data.Map' returns results" $ do
      dbPath <- getDbPath
      withHoogleDb dbPath $ \databaseRef -> do
        result <- handleTool databaseRef (LookupModule (LookupModuleParams "Data.Map"))
        assertBool "module lookup should return results"
          (result /= "No results found.")
  ]

-- | Load a hoogle database and run an action with it
withHoogleDb :: FilePath -> (IORef Database -> IO ()) -> IO ()
withHoogleDb path action =
  withDatabase path $ \database -> do
    databaseRef <- newIORef database
    action databaseRef

-- | Helper to create a Target for testing
mkTarget :: String -> Maybe (String, String) -> Maybe (String, String) -> Target
mkTarget item package moduleDef = Target
  { targetURL = "https://hackage.haskell.org/example"
  , targetPackage = package
  , targetModule = moduleDef
  , targetType = ""
  , targetItem = item
  , targetDocs = ""
  }
