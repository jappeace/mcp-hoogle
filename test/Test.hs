module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text qualified as Text
import McpHoogle.Format (formatTarget, formatTargets, stripHtmlTags)
import Hoogle (Target(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "McpHoogle"
  [ formatTests
  , stripHtmlTests
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
