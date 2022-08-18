{
  description = "casper";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs = self.haskellPackages.extend (hfinal: hprev: {
            casper = hfinal.callCabal2nix "casper" ./. { };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };
      in
      {
        defaultPackage = pkgs.hsPkgs.casper;
        devShell = pkgs.hsPkgs.shellFor {
          packages = ps: [ ps.casper ];
          withHoogle = true;
          buildInputs = [
            pkgs.ormolu
            pkgs.hlint
            pkgs.haskell-language-server
            pkgs.cabal-install
          ];
        };
      }
    );
}
