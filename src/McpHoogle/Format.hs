module McpHoogle.Format
  ( formatTarget
  , formatTargets
  , stripHtmlTags
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Hoogle (Target(..))

-- | Format a single Hoogle Target into readable text
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

-- | Format multiple targets, numbered
formatTargets :: [Target] -> Text
formatTargets [] = "No results found."
formatTargets targets =
  Text.intercalate "\n---\n\n" (map formatTarget targets)

-- | Strip HTML tags from a string (simple implementation)
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
