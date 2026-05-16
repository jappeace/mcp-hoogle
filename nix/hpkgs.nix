{ pkgs ? import ./pkgs.nix { }
,
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    mcp-hoogle = pkgs.haskell.lib.enableCabalFlag
      (hnew.callCabal2nix "mcp-hoogle" ../. { })
      "werror";
  };
}
