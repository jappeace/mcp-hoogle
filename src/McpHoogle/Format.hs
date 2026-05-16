-- | Rendering of Hoogle search results into human-readable text.
--
-- Hoogle's 'Target' type contains HTML markup (e.g. @\<s0\>map\<\/s0\>@)
-- which is meaningless in a plain-text MCP response. This module strips
-- the HTML and formats each result with its package, module, docs, and URL
-- so an AI assistant can read them directly.
module McpHoogle.Format
  ( formatTarget
  , formatTargets
  , stripHtmlTags
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Hoogle (Target(..))

-- | Format a single Hoogle 'Target' into a readable markdown-ish block.
--
-- Includes the item signature (HTML-stripped), package name, module name,
-- documentation excerpt, and a link to the Hackage page.
formatTarget :: Target -> Text
formatTarget target = Text.unlines
  [ "## " <> stripHtmlTags (Text.pack (targetItem target))
  , case targetPackage target of
      Just (package, _url) -> "Package: " <> Text.pack package
      Nothing -> ""
  , case targetModule target of
      Just (moduleName, _url) -> "Module: " <> Text.pack moduleName
      Nothing -> ""
  , if null (targetDocs target)
      then ""
      else "\n" <> stripHtmlTags (Text.pack (targetDocs target))
  , "URL: " <> Text.pack (targetURL target)
  ]

-- | Format a list of targets separated by horizontal rules.
-- Returns a "No results found." message for an empty list.
formatTargets :: [Target] -> Text
formatTargets [] = "No results found."
formatTargets targets =
  Text.intercalate "\n---\n\n" (map formatTarget targets)

-- | Strip HTML tags from text, keeping only the text content.
--
-- Hoogle wraps function names in @\<s0\>...\<\/s0\>@ spans and uses
-- other HTML for formatting. This does a single-pass removal of all
-- angle-bracketed sequences.
stripHtmlTags :: Text -> Text
stripHtmlTags = go False
  where
    go :: Bool -> Text -> Text
    go _inTag input = case Text.uncons input of
      Nothing -> Text.empty
      Just ('<', rest) -> go True rest
      Just (char, rest)
        | _inTag -> case char of
            '>' -> go False rest
            _   -> go True rest
        | otherwise -> Text.cons char (go False rest)
