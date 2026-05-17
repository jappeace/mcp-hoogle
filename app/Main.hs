module Main where

import McpHoogle (runServer, runServerWithDb)
import Hoogle (defaultDatabaseLocation, hoogle)
import Options.Applicative
import Data.Version (showVersion)
import Paths_mcp_hoogle (version)

data Command
  = Generate GenerateOpts
  | Serve ServeOpts

data GenerateOpts = GenerateOpts
  { generateDatabase :: Maybe FilePath
  }

data ServeOpts = ServeOpts
  { serveDatabase :: Maybe FilePath
  }

commandParser :: Parser Command
commandParser = subparser
  ( command "generate" (info (Generate <$> generateOptsParser) (progDesc "Generate Hoogle DB from local GHC packages"))
  <> command "serve" (info (Serve <$> serveOptsParser) (progDesc "Run MCP server (stdio transport)"))
  ) <|> (Serve <$> serveOptsParser)

generateOptsParser :: Parser GenerateOpts
generateOptsParser = GenerateOpts
  <$> optional (strOption
    ( long "database"
    <> short 'd'
    <> metavar "PATH"
    <> help "Path to write the Hoogle database (default: ~/.hoogle/)"
    ))

serveOptsParser :: Parser ServeOpts
serveOptsParser = ServeOpts
  <$> optional (strOption
    ( long "database"
    <> short 'd'
    <> metavar "PATH"
    <> help "Path to the Hoogle database file"
    ))

opts :: ParserInfo Command
opts = info (commandParser <**> helper <**> simpleVersioner ("mcp-hoogle " <> showVersion version))
  ( fullDesc
  <> progDesc "MCP server exposing Hoogle search over local Haskell dependencies"
  <> header "mcp-hoogle - Hoogle search via Model Context Protocol"
  )

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Generate (GenerateOpts mPath) -> do
      databasePath <- maybe defaultDatabaseLocation pure mPath
      putStrLn $ "Generating Hoogle database at: " <> databasePath
      putStrLn "Indexing local packages from GHC package database..."
      hoogle ["generate", "--local", "--database=" <> databasePath]
      putStrLn "Done."
    Serve (ServeOpts mPath) ->
      case mPath of
        Nothing -> runServer
        Just path -> runServerWithDb path
