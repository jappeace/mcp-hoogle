module McpHoogle.Tools
  ( HoogleTool(..)
  , SearchParams(..)
  , SearchTypeParams(..)
  , LookupModuleParams(..)
  , handleTool
  , toolDescriptions
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Hoogle (Database, searchDatabase)
import McpHoogle.Format (formatTargets)

-- | Parameter records for each tool
data SearchParams = SearchParams
  { query :: Text
  }

data SearchTypeParams = SearchTypeParams
  { typeSignature :: Text
  }

data LookupModuleParams = LookupModuleParams
  { moduleName :: Text
  }

-- | MCP tools exposed by this server.
-- Uses nested parameter types to avoid partial record selectors.
data HoogleTool
  = Search SearchParams
  | SearchType SearchTypeParams
  | LookupModule LookupModuleParams

-- | Descriptions for TH derivation
toolDescriptions :: [(String, String)]
toolDescriptions =
  [ ("Search", "Search Hoogle by function name, type signature, or keyword. Returns matching functions with their types, packages, and documentation.")
  , ("SearchType", "Search Hoogle specifically by type signature. Example: 'a -> [a]' or '[a] -> Int'")
  , ("LookupModule", "Search for all exports of a given module name. Example: 'Data.Map'")
  , ("query", "The search query: a function name, keyword, or type signature")
  , ("typeSignature", "A Haskell type signature to search for, e.g. '[a] -> Int' or 'Map k v -> [(k,v)]'")
  , ("moduleName", "A Haskell module name to look up, e.g. 'Data.Map' or 'Control.Monad'")
  ]

-- | Handle a tool call by searching the Hoogle database
handleTool :: Database -> HoogleTool -> IO Text
handleTool database (Search (SearchParams searchQuery)) = do
  let results = take 20 (searchDatabase database (Text.unpack searchQuery))
  pure (formatTargets results)
handleTool database (SearchType (SearchTypeParams sig)) = do
  let results = take 20 (searchDatabase database (Text.unpack sig))
  pure (formatTargets results)
handleTool database (LookupModule (LookupModuleParams modName)) = do
  let queryString = "module:" <> Text.unpack modName
      results = take 30 (searchDatabase database queryString)
  -- If module: prefix doesn't work well, fall back to plain search
  let finalResults = if null results
        then take 30 (searchDatabase database (Text.unpack modName))
        else results
  pure (formatTargets finalResults)
