{ pkgs ? import ./pkgs.nix { }
,
}:
let
  # Use our fork with the protocol negotiation fix:
  # https://github.com/drshade/haskell-mcp-server/pull/11
  # Upstream 0.1.0.19 always responds with "2025-06-18" regardless of what
  # the client proposes, causing Claude Code (which sends "2024-11-05") to
  # disconnect and tools never appear.
  mcp-server-src = pkgs.fetchFromGitHub {
    owner = "jappeace-sloth";
    repo = "haskell-mcp-server";
    rev = "8572533";
    hash = "sha256-hWJWedD6kXpK2ozgksVO+IanmAJNbzWwOK6XqdnZBw8=";
  };
in
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    mcp-server = pkgs.haskell.lib.dontCheck
      (hnew.callCabal2nix "mcp-server" mcp-server-src { });
    mcp-hoogle = pkgs.haskell.lib.enableCabalFlag
      (hnew.callCabal2nix "mcp-hoogle" ../. { })
      "werror";
  };
}
