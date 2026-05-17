-- | MCP tool definitions and their handlers.
--
-- Each constructor of 'HoogleTool' becomes an MCP tool that Claude (or any
-- MCP client) can invoke. The @mcp-server@ library's Template Haskell
-- derivation turns the ADT into a tool list + dispatcher automatically.
--
-- The handler reads from an 'IORef' holding a 'Maybe' 'Database'. The ref
-- starts as 'Nothing' when no database exists on disk and is populated
-- by 'RegenerateDatabase' or 'ReloadDatabase'.
module McpHoogle.Tools
  ( HoogleTool(..)
  , SearchParams(..)
  , SearchTypeParams(..)
  , LookupModuleParams(..)
  , RegenerateDatabaseParams(..)
  , ReloadDatabaseParams(..)
  , handleTool
  , toolDescriptions
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.IORef (IORef, readIORef, writeIORef)
import Hoogle (Database, searchDatabase, withDatabase, hoogle, defaultDatabaseLocation)
import McpHoogle.Format (formatTargets)
import System.Environment (setEnv, lookupEnv)
import System.IO (stdout, stderr, hFlush)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Control.Exception (bracket)

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
--
-- Requires @ghcBinPath@ so that @ghc-pkg@ can be found even when the
-- MCP server was started outside a nix-shell.
data RegenerateDatabaseParams = RegenerateDatabaseParams
  { ghcBinPath :: Text
  }

-- | Parameters for reloading the database from disk without regenerating.
data ReloadDatabaseParams = ReloadDatabaseParams
  { reloadPath :: Text
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
  | ReloadDatabase ReloadDatabaseParams

-- | Human-readable descriptions for each tool and its arguments.
-- Fed to 'deriveToolHandlerWithDescription' so MCP clients know what
-- each tool does and what arguments to pass.
toolDescriptions :: [(String, String)]
toolDescriptions =
  [ ("Search", "Search Hoogle by function name, type signature, or keyword. Returns matching functions with their types, packages, and documentation.")
  , ("SearchType", "Search Hoogle specifically by type signature. Example: 'a -> [a]' or '[a] -> Int'")
  , ("LookupModule", "Search for all exports of a given module name. Example: 'Data.Map'")
  , ("RegenerateDatabase", "Regenerate the Hoogle database by indexing packages from the GHC package database. Requires ghcBinPath so ghc-pkg can be found. Find it with: nix-shell --run 'dirname $(which ghc-pkg)'")
  , ("ReloadDatabase", "Reload the Hoogle database from disk without regenerating. Use after running 'mcp-hoogle generate' externally via a bash command.")
  , ("query", "The search query: a function name, keyword, or type signature")
  , ("typeSignature", "A Haskell type signature to search for, e.g. '[a] -> Int' or 'Map k v -> [(k,v)]'")
  , ("moduleName", "A Haskell module name to look up, e.g. 'Data.Map' or 'Control.Monad'")
  , ("ghcBinPath", "Path to GHC's bin directory containing ghc-pkg. Find with: nix-shell --run 'dirname $(which ghc-pkg)'")
  , ("reloadPath", "Path to the .hoo database file to reload. Use empty string for default (~/.hoogle/default-haskell-5.0.18.hoo).")
  ]

-- | Dispatch a tool call to the appropriate Hoogle operation.
--
-- Reads the current database from the 'IORef'. For 'RegenerateDatabase',
-- temporarily prepends the given GHC bin path to PATH, runs
-- @hoogle generate --local@, reloads the resulting file, and swaps the
-- 'IORef' contents so subsequent searches use the new data.
handleTool :: IORef (Maybe Database) -> HoogleTool -> IO Text
handleTool databaseRef (Search (SearchParams searchQuery)) = do
  mDatabase <- readIORef databaseRef
  case mDatabase of
    Nothing -> pure "No database loaded. Call regenerate_database with your project's ghcBinPath first."
    Just database -> do
      let results = take 20 (searchDatabase database (Text.unpack searchQuery))
      pure (formatTargets results)
handleTool databaseRef (SearchType (SearchTypeParams sig)) = do
  mDatabase <- readIORef databaseRef
  case mDatabase of
    Nothing -> pure "No database loaded. Call regenerate_database with your project's ghcBinPath first."
    Just database -> do
      let results = take 20 (searchDatabase database (Text.unpack sig))
      pure (formatTargets results)
handleTool databaseRef (LookupModule (LookupModuleParams modName)) = do
  mDatabase <- readIORef databaseRef
  case mDatabase of
    Nothing -> pure "No database loaded. Call regenerate_database with your project's ghcBinPath first."
    Just database -> do
      let queryString = "module:" <> Text.unpack modName
          results = take 30 (searchDatabase database queryString)
      -- If module: prefix doesn't work well, fall back to plain search
      let finalResults = if null results
            then take 30 (searchDatabase database (Text.unpack modName))
            else results
      pure (formatTargets finalResults)
handleTool databaseRef (RegenerateDatabase (RegenerateDatabaseParams ghcBin)) = do
  dbPath <- defaultDatabaseLocation
  let ghcBinStr = Text.unpack ghcBin
  withPrependedPath ghcBinStr $ do
    withSilencedStdout $ do
      hoogle ["generate", "--local", "--database=" <> dbPath]
  withDatabase dbPath $ \newDb -> do
    writeIORef databaseRef (Just newDb)
    pure (Text.pack ("Database regenerated and reloaded from: " <> dbPath))
handleTool databaseRef (ReloadDatabase (ReloadDatabaseParams path)) = do
  dbPath <- if Text.null path
    then defaultDatabaseLocation
    else pure (Text.unpack path)
  withDatabase dbPath $ \newDb -> do
    writeIORef databaseRef (Just newDb)
    pure (Text.pack ("Database reloaded from: " <> dbPath))

-- | Temporarily prepend a directory to PATH, run an action, then restore.
withPrependedPath :: FilePath -> IO a -> IO a
withPrependedPath dir action = do
  oldPath <- lookupEnv "PATH"
  let newPath = case oldPath of
        Nothing -> dir
        Just p  -> dir <> ":" <> p
  bracket
    (setEnv "PATH" newPath)
    (\_ -> maybe (setEnv "PATH" "") (setEnv "PATH") oldPath)
    (\_ -> action)

-- | Redirect stdout to \/dev\/null during an action.
--
-- Hoogle's library API writes progress text to stdout which would corrupt
-- the MCP JSON-RPC stream. We redirect stdout to stderr (so it's still
-- visible for debugging) and restore it after.
withSilencedStdout :: IO a -> IO a
withSilencedStdout action = do
  hFlush stdout
  savedStdout <- hDuplicate stdout
  hDuplicateTo stderr stdout
  result <- action
  hFlush stdout
  hDuplicateTo savedStdout stdout
  pure result
