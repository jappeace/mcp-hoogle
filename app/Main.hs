module Main where

import McpHoogle (runServer, runServerWithDb)
import System.Environment (getArgs)
import System.Process (callProcess)
import Hoogle (defaultDatabaseLocation)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["generate"] -> do
      databasePath <- defaultDatabaseLocation
      putStrLn $ "Generating Hoogle database at: " <> databasePath
      putStrLn "Indexing local packages from GHC package database..."
      callProcess "hoogle" ["generate", "--local", "--database=" <> databasePath]
      putStrLn "Done."
    ["generate", "--database", path] -> do
      putStrLn $ "Generating Hoogle database at: " <> path
      putStrLn "Indexing local packages from GHC package database..."
      callProcess "hoogle" ["generate", "--local", "--database=" <> path]
      putStrLn "Done."
    ["serve"] -> runServer
    ["serve", databasePath] -> runServerWithDb databasePath
    [] -> runServer
    [databasePath] -> runServerWithDb databasePath
    _ -> do
      putStrLn "Usage: mcp-hoogle [command]"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn "  generate                  Generate Hoogle DB from local GHC packages"
      putStrLn "  generate --database PATH  Generate Hoogle DB at specific path"
      putStrLn "  serve [PATH]              Run MCP server (default command)"
      putStrLn ""
      putStrLn "Run from within a nix-shell to index project dependencies."
