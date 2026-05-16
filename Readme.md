[![Github actions build status](https://img.shields.io/github/actions/workflow/status/jappeace/mcp-hoogle/ci.yaml?branch=master)](https://github.com/jappeace/mcp-hoogle/actions)

# mcp-hoogle

An MCP (Model Context Protocol) server that exposes Hoogle search over your project's local Haskell dependencies.
Run it from within your project's nix-shell to give AI assistants type-aware search across all your project's packages.

## Usage

### 1. Generate a Hoogle database from your project

From within your project's nix-shell (where all dependencies are available):

```bash
mcp-hoogle generate
```

This indexes all packages in your local GHC package database.

### 2. Run the MCP server

```bash
mcp-hoogle serve
```

Or with a specific database path:

```bash
mcp-hoogle serve /path/to/database.hoo
```

### 3. Configure Claude Code

Add to your Claude Code MCP settings:

```json
{
  "mcpServers": {
    "hoogle": {
      "command": "mcp-hoogle",
      "args": ["serve"]
    }
  }
}
```

## MCP Tools

The server exposes three tools:

- **search** — Search by function name, type signature, or keyword
- **search_type** — Search specifically by type signature (e.g. `[a] -> Int`)
- **lookup_module** — Browse exports of a module (e.g. `Data.Map`)

## Building

```bash
nix-shell
cabal build
```

Or via nix:

```bash
nix-build
```

## How it works

1. `mcp-hoogle generate` calls `hoogle generate --local` which indexes all packages registered in the current GHC package database
2. `mcp-hoogle serve` loads the generated Hoogle database and exposes it via MCP stdio transport
3. AI assistants connect via MCP and can search for types, functions, and modules
