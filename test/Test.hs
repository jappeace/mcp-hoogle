module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.IORef (IORef, newIORef)
import Data.Text qualified as Text
import Hoogle (Database, Target(..), defaultDatabaseLocation, withDatabase)
import McpHoogle.Format (formatTarget, formatTargets, stripHtmlTags)
import McpHoogle.Tools (HoogleTool(..), SearchParams(..), SearchTypeParams(..), LookupModuleParams(..), handleTool)
import System.Directory (doesFileExist)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "McpHoogle"
  [ formatTests
  , stripHtmlTests
  , hoogleSearchTests
  ]

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
-- These tests require a pre-generated hoogle database and are skipped
-- when the database doesn't exist (e.g. in nix build sandboxes).
hoogleSearchTests :: TestTree
hoogleSearchTests = testGroup "Hoogle search integration"
  [ testCase "search for 'map' returns results" $ do
      withHoogleDb $ \databaseRef -> do
        result <- handleTool databaseRef (Search (SearchParams "map"))
        assertBool "search should return results, not 'No results found'"
          (result /= "No results found.")
        assertBool "search for map should mention 'map' in results"
          (Text.isInfixOf "map" result)
  , testCase "type search '[a] -> Int' returns results" $ do
      withHoogleDb $ \databaseRef -> do
        result <- handleTool databaseRef (SearchType (SearchTypeParams "[a] -> Int"))
        assertBool "type search should return results"
          (result /= "No results found.")
  , testCase "module lookup 'Data.Map' returns results" $ do
      withHoogleDb $ \databaseRef -> do
        result <- handleTool databaseRef (LookupModule (LookupModuleParams "Data.Map"))
        assertBool "module lookup should return results"
          (result /= "No results found.")
  ]

-- | Run a test with the hoogle database, skipping if DB doesn't exist
withHoogleDb :: (IORef Database -> IO ()) -> IO ()
withHoogleDb action = do
  dbPath <- defaultDatabaseLocation
  dbExists <- doesFileExist dbPath
  if dbExists
    then withDatabase dbPath $ \database -> do
      databaseRef <- newIORef database
      action databaseRef
    else putStrLn $ "  [SKIPPED] Hoogle database not found at " <> dbPath

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
