{-# LANGUAGE TemplateHaskell #-}
-- | Entry point for the MCP server.
--
-- This module wires together the Hoogle database, the tool definitions from
-- "McpHoogle.Tools", and the @mcp-server@ library's stdio transport.
-- It loads the Hoogle database into an 'IORef' so it can be swapped at
-- runtime (via the @regenerate_database@ tool), then enters the MCP
-- request\/response loop reading JSON-RPC from stdin and writing to stdout.
module McpHoogle
  ( runServer
  , runServerWithDb
  )
where

import Data.IORef (newIORef)
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
-- Errors immediately if no database is found — the user should run
-- @mcp-hoogle generate@ first.
runServer :: IO ()
runServer = do
  defaultPath <- defaultDatabaseLocation
  exists <- doesFileExist defaultPath
  if exists
    then runServerWithDb defaultPath
    else error $ "Hoogle database not found at: " <> defaultPath
      <> "\nRun 'mcp-hoogle generate' to create it, or pass --database PATH."

-- | Run the MCP server with an explicit database path.
--
-- Loads the database, stores it in an 'IORef' (for hot-reload support),
-- registers the tool handlers via TH-derived dispatch, and enters the
-- stdio MCP loop. This function blocks until stdin is closed.
runServerWithDb :: FilePath -> IO ()
runServerWithDb databasePath =
  withDatabase databasePath $ \database -> do
    databaseRef <- newIORef database
    let serverInfo :: McpServerInfo
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
              , "- regenerate_database: Re-index after entering a different project's nix-shell"
              , ""
              , "DATABASE SETUP:"
              , "If no database exists, run `mcp-hoogle generate` from an"
              , "environment where `ghc-pkg` is on PATH (so it can discover"
              , "installed packages). For nix-based projects this means running"
              , "from inside the project's nix-shell:"
              , "  nix-shell --run 'mcp-hoogle generate'"
              , "For cabal/stack projects, just run `mcp-hoogle generate` directly"
              , "(GHC tools are already on PATH)."
              , "This only needs to be done once per project. The database persists"
              , "at ~/.hoogle/ and is reused across sessions."
              ]
          }

        toolHandler :: HoogleTool -> IO Content
        toolHandler tool = ContentText <$> handleTool databaseRef tool

        handlers :: McpServerHandlers IO
        handlers = McpServerHandlers
          { prompts = Nothing
          , resources = Nothing
          , tools = Just $(deriveToolHandlerWithDescription ''HoogleTool 'toolHandler toolDescriptions)
          }

    runMcpServerStdio serverInfo handlers
