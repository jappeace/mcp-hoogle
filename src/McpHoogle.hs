{-# LANGUAGE TemplateHaskell #-}
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

-- | Run the MCP server using the default Hoogle database location.
runServer :: IO ()
runServer = do
  defaultPath <- defaultDatabaseLocation
  exists <- doesFileExist defaultPath
  if exists
    then runServerWithDb defaultPath
    else error $ "Hoogle database not found at: " <> defaultPath
      <> "\nRun 'mcp-hoogle generate' to create it, or pass --database PATH."

-- | Run the MCP server with an explicit database path
runServerWithDb :: FilePath -> IO ()
runServerWithDb databasePath =
  withDatabase databasePath $ \database -> do
    databaseRef <- newIORef database
    let serverInfo :: McpServerInfo
        serverInfo = McpServerInfo
          { serverName = "mcp-hoogle"
          , serverVersion = "0.1.0"
          , serverInstructions = Text.unlines
              [ "Hoogle search server for Haskell."
              , "Search by function name, type signature, or browse modules."
              , "The database is built from the project's local dependencies."
              , "Use regenerate_database to reload after switching projects."
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
