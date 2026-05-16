{-# LANGUAGE TemplateHaskell #-}
module McpHoogle
  ( runServer
  , runServerWithDb
  )
where

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

-- | Run the MCP server using the default Hoogle database location.
-- Falls back to a provided path if the default doesn't exist.
runServer :: IO ()
runServer = do
  defaultPath <- defaultDatabaseLocation
  exists <- doesFileExist defaultPath
  if exists
    then runServerWithDb defaultPath
    else error $ "Hoogle database not found at: " <> defaultPath
      <> "\nRun 'hoogle generate' to create it, or pass a path via command line."

-- | Run the MCP server with an explicit database path
runServerWithDb :: FilePath -> IO ()
runServerWithDb databasePath =
  withDatabase databasePath $ \database -> do
    let serverInfo :: McpServerInfo
        serverInfo = McpServerInfo
          { serverName = "mcp-hoogle"
          , serverVersion = "0.1.0"
          , serverInstructions = Text.unlines
              [ "Hoogle search server for Haskell."
              , "Search by function name, type signature, or browse modules."
              , "The database is built from the project's local dependencies."
              ]
          }

        toolHandler :: HoogleTool -> IO Content
        toolHandler tool = ContentText <$> handleTool database tool

        handlers :: McpServerHandlers IO
        handlers = McpServerHandlers
          { prompts = Nothing
          , resources = Nothing
          , tools = Just $(deriveToolHandlerWithDescription ''HoogleTool 'toolHandler toolDescriptions)
          }

    runMcpServerStdio serverInfo handlers
