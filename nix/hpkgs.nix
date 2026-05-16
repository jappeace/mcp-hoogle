{ pkgs ? import ./pkgs.nix { }
,
}:
let
  # mcp-server 0.1.0.17+ supports protocol version negotiation which
  # is required for Claude Code compatibility (it sends 2024-11-05).
  mcp-server-src = pkgs.fetchurl {
    url = "https://hackage.haskell.org/package/mcp-server-0.1.0.19/mcp-server-0.1.0.19.tar.gz";
    hash = "sha256-riZRTiT6TcESyRRMK5+w9v1xu7okgFHaBxH7ltqScYc=";
  };
in
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    mcp-server = pkgs.haskell.lib.dontCheck
      (hnew.callCabal2nix "mcp-server" (pkgs.runCommand "mcp-server-src" {} ''
        mkdir $out
        tar xzf ${mcp-server-src} --strip-components=1 -C $out
      '') { });
    mcp-hoogle = pkgs.haskell.lib.enableCabalFlag
      (hnew.callCabal2nix "mcp-hoogle" ../. { })
      "werror";
  };
}
