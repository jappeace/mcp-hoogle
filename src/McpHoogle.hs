{-# LANGUAGE TemplateHaskell #-}
-- | Entry point for the MCP server.
--
-- This module wires together the Hoogle database, the tool definitions from
-- "McpHoogle.Tools", and the @mcp-server@ library's stdio transport.
-- It loads the Hoogle database into an 'IORef' so it can be swapped at
-- runtime (via the @regenerate_database@ or @reload_database@ tools),
-- then enters the MCP request\/response loop reading JSON-RPC from stdin
-- and writing to stdout.
--
-- The server starts even without an existing database — searches return
-- "No database loaded" until one is generated or reloaded.
module McpHoogle
  ( runServer
  , runServerWithDb
  )
where

import Data.IORef (IORef, newIORef)
import Hoogle qualified (Database)
import Data.Text qualified as Text
import Hoogle (withDatabase, defaultDatabaseLocation)
import MCP.Server (runMcpServerStdio)
import MCP.Server.Types
  ( McpServerInfo(..)
  , McpServerHandlers(..)
  , Content(..)
  )
import MCP.Server.Derive (deriveToolHandlerWithDescription)
import McpHoogle.Tools (HoogleTool(..), handleTool, toolDescriptions)
import System.Directory (doesFileExist)

-- | Run the MCP server using the default Hoogle database location
-- (@~\/.hoogle\/default-haskell-*.hoo@).
--
-- If no database exists, the server starts anyway with an empty database
-- ref — the agent can call @regenerate_database@ or @reload_database@ to
-- populate it without restarting.
runServer :: IO ()
runServer = do
  defaultPath <- defaultDatabaseLocation
  exists <- doesFileExist defaultPath
  if exists
    then runServerWithDb defaultPath
    else runServerEmpty

-- | Run the MCP server with an explicit database path.
--
-- Loads the database, stores it in an 'IORef' (for hot-reload support),
-- registers the tool handlers via TH-derived dispatch, and enters the
-- stdio MCP loop. This function blocks until stdin is closed.
runServerWithDb :: FilePath -> IO ()
runServerWithDb databasePath =
  withDatabase databasePath $ \database -> do
    databaseRef <- newIORef (Just database)
    runMcpServerStdio serverInfo (handlers databaseRef)

-- | Run the MCP server without a database.
--
-- The server starts and exposes tools, but searches return "No database
-- loaded" until the agent calls @regenerate_database@ or @reload_database@.
runServerEmpty :: IO ()
runServerEmpty = do
  databaseRef <- newIORef Nothing
  runMcpServerStdio serverInfo (handlers databaseRef)

-- | Server metadata sent during MCP initialization.
serverInfo :: McpServerInfo
serverInfo = McpServerInfo
  { serverName = "mcp-hoogle"
  , serverVersion = "0.1.0"
  , serverInstructions = Text.unlines
      [ "Hoogle search for Haskell types, functions, and modules."
      , ""
      , "USE THESE TOOLS INSTEAD OF:"
      , "- Web searching for Haskell documentation"
      , "- Running `hoogle` or `mcp-hoogle` CLI commands"
      , "- Fetching Hackage pages with curl/w3m"
      , ""
      , "AVAILABLE TOOLS:"
      , "- search: Find functions by name, keyword, or type signature (e.g. \"map\", \"[a] -> Int\")"
      , "- search_type: Search specifically by type signature"
      , "- lookup_module: Browse all exports of a module (e.g. \"Data.Map\")"
      , "- regenerate_database: Re-index packages. Pass ghcBinPath (find it with: nix-shell --run 'dirname $(which ghc-pkg)')"
      , "- reload_database: Reload database from disk after generating externally"
      , ""
      , "DATABASE SETUP:"
      , "If searches return 'No database loaded', call regenerate_database with"
      , "the ghcBinPath from the project's nix-shell. Find it by running:"
      , "  nix-shell --run 'dirname $(which ghc-pkg)'"
      , "Then pass that path to regenerate_database."
      , "Alternatively, run `nix-shell --run 'mcp-hoogle generate'` as a bash"
      , "command, then call reload_database to pick up the new file."
      ]
  }

-- | Build MCP handlers from a database ref.
handlers :: IORef (Maybe Hoogle.Database) -> McpServerHandlers IO
handlers databaseRef = McpServerHandlers
  { prompts = Nothing
  , resources = Nothing
  , tools = Just $(deriveToolHandlerWithDescription ''HoogleTool 'toolHandler toolDescriptions)
  }
  where
    toolHandler :: HoogleTool -> IO Content
    toolHandler tool = ContentText <$> handleTool databaseRef tool
