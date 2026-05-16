{ pkgs ? import ./pkgs.nix { }
,
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    mcp-hoogle = hnew.callCabal2nix "mcp-hoogle" ../. { };
  };
}
