module McpHoogle.Tools
  ( HoogleTool(..)
  , SearchParams(..)
  , SearchTypeParams(..)
  , LookupModuleParams(..)
  , RegenerateDatabaseParams(..)
  , handleTool
  , toolDescriptions
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.IORef (IORef, readIORef, writeIORef)
import Hoogle (Database, searchDatabase, withDatabase)
import McpHoogle.Format (formatTargets)
import System.Process (callProcess)

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

data RegenerateDatabaseParams = RegenerateDatabaseParams
  { databasePath :: Text
  }

-- | MCP tools exposed by this server.
-- Uses nested parameter types to avoid partial record selectors.
data HoogleTool
  = Search SearchParams
  | SearchType SearchTypeParams
  | LookupModule LookupModuleParams
  | RegenerateDatabase RegenerateDatabaseParams

-- | Descriptions for TH derivation
toolDescriptions :: [(String, String)]
toolDescriptions =
  [ ("Search", "Search Hoogle by function name, type signature, or keyword. Returns matching functions with their types, packages, and documentation.")
  , ("SearchType", "Search Hoogle specifically by type signature. Example: 'a -> [a]' or '[a] -> Int'")
  , ("LookupModule", "Search for all exports of a given module name. Example: 'Data.Map'")
  , ("RegenerateDatabase", "Regenerate the Hoogle database from the current GHC package database. Call this after switching projects or nix-shells to re-index available packages. The databasePath should be the path to write the .hoo file.")
  , ("query", "The search query: a function name, keyword, or type signature")
  , ("typeSignature", "A Haskell type signature to search for, e.g. '[a] -> Int' or 'Map k v -> [(k,v)]'")
  , ("moduleName", "A Haskell module name to look up, e.g. 'Data.Map' or 'Control.Monad'")
  , ("databasePath", "Path to the Hoogle database file to regenerate and reload")
  ]

-- | Handle a tool call by searching the Hoogle database.
-- The IORef allows the database to be swapped on regeneration.
handleTool :: IORef Database -> HoogleTool -> IO Text
handleTool databaseRef (Search (SearchParams searchQuery)) = do
  database <- readIORef databaseRef
  let results = take 20 (searchDatabase database (Text.unpack searchQuery))
  pure (formatTargets results)
handleTool databaseRef (SearchType (SearchTypeParams sig)) = do
  database <- readIORef databaseRef
  let results = take 20 (searchDatabase database (Text.unpack sig))
  pure (formatTargets results)
handleTool databaseRef (LookupModule (LookupModuleParams modName)) = do
  database <- readIORef databaseRef
  let queryString = "module:" <> Text.unpack modName
      results = take 30 (searchDatabase database queryString)
  -- If module: prefix doesn't work well, fall back to plain search
  let finalResults = if null results
        then take 30 (searchDatabase database (Text.unpack modName))
        else results
  pure (formatTargets finalResults)
handleTool databaseRef (RegenerateDatabase (RegenerateDatabaseParams path)) = do
  let pathStr = Text.unpack path
  callProcess "hoogle" ["generate", "--local", "--database=" <> pathStr]
  withDatabase pathStr $ \newDb -> do
    writeIORef databaseRef newDb
    pure "Database regenerated and reloaded successfully."
