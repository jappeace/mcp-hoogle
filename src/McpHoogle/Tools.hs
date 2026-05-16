-- | MCP tool definitions and their handlers.
--
-- Each constructor of 'HoogleTool' becomes an MCP tool that Claude (or any
-- MCP client) can invoke. The @mcp-server@ library's Template Haskell
-- derivation turns the ADT into a tool list + dispatcher automatically.
--
-- The handler reads from an 'IORef' 'Database' so the database can be
-- hot-swapped via 'RegenerateDatabase' without restarting the server.
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
import Hoogle (Database, searchDatabase, withDatabase, hoogle)
import McpHoogle.Format (formatTargets)

-- | Parameters for a general search (name, keyword, or type signature).
data SearchParams = SearchParams
  { query :: Text
  }

-- | Parameters for a type-signature-specific search.
data SearchTypeParams = SearchTypeParams
  { typeSignature :: Text
  }

-- | Parameters for a module-name lookup.
data LookupModuleParams = LookupModuleParams
  { moduleName :: Text
  }

-- | Parameters for database regeneration.
data RegenerateDatabaseParams = RegenerateDatabaseParams
  { databasePath :: Text
  }

-- | The set of MCP tools this server exposes.
--
-- Each constructor maps to one callable tool. The nested parameter records
-- provide named arguments (avoiding partial record selectors) which the
-- TH derivation exposes as the tool's input schema.
data HoogleTool
  = Search SearchParams
  | SearchType SearchTypeParams
  | LookupModule LookupModuleParams
  | RegenerateDatabase RegenerateDatabaseParams

-- | Human-readable descriptions for each tool and its arguments.
-- Fed to 'deriveToolHandlerWithDescription' so MCP clients know what
-- each tool does and what arguments to pass.
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

-- | Dispatch a tool call to the appropriate Hoogle operation.
--
-- Reads the current database from the 'IORef'. For 'RegenerateDatabase',
-- shells out to @hoogle generate --local@, reloads the resulting file,
-- and swaps the 'IORef' contents so subsequent searches use the new data.
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
  hoogle ["generate", "--local", "--database=" <> pathStr]
  withDatabase pathStr $ \newDb -> do
    writeIORef databaseRef newDb
    pure "Database regenerated and reloaded successfully."
