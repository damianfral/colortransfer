{
  description = "A tool to transfer color between images.";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, ... }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          nix-filter.overlays.default
        ];
      };
    in

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = pkgsFor system;
        filteredSrc =
          pkgs.nix-filter {
            root = ./.;
            include = [
              "src/"
              "test/"
              "package.yaml"
              "LICENSE"
            ];
          };
      in
      rec {
        packages = {
          colortransfer = pkgs.haskell.lib.justStaticExecutables (
            pkgs.haskellPackages.colortransfer
          );
        };

        defaultPackage = packages.colortransfer;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ packages.colortransfer ];
          buildInputs = with pkgs; with pkgs.haskellPackages; [
            haskell-language-server
            cabal-install
            ghcid
            hpack
            hlint
          ];
        };

        overlays = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                colortransfer = self.generateOptparseApplicativeCompletions
                  [ "colortransfer" ]
                  (self.callCabal2nix "colortransfer" filteredSrc { });
              }
              );
          });
        };
      });
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}

