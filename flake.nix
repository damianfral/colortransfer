{
  description = "A tool to transfer color between images.";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/24.05"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, pre-commit-hooks, feedback, ... }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          nix-filter.overlays.default
        ];
      };
      filteredSrc =
        nix-filter.lib {
          root = ./.;
          include = [
            "src/"
            "test/"
            "package.yaml"
            "LICENSE"
            "test-resources/"
          ];
        };

    in
    {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            (self: super: {
              hmatrix = super.hmatrix.overrideAttrs (drv: {
                configureFlags = (drv.configureFlags or [ ]) ++ [ "-fopenblas" ];
                extraLibraries = [ final.openblasCompat ];
                preConfigure = ''
                  sed -i hmatrix.cabal -e 's@/usr/lib/openblas/lib@${final.openblasCompat}/lib@'
                '';
              });
              colortransfer = self.generateOptparseApplicativeCompletions
                [ "colortransfer" ]
                ((self.callCabal2nix "colortransfer" filteredSrc { }).overrideAttrs (oldAttrs: {
                  preBuild = ''
                    ln -s ${filteredSrc}/test-resources $PWD/test-resources
                  '';
                }));
            });
        });
      };
    }
    //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
          };
        };
      in
      rec {
        packages = rec {
          colortransfer = pkgs.haskell.lib.justStaticExecutables (
            pkgs.haskellPackages.colortransfer
          );
          default = colortransfer;
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ packages.colortransfer ];
          buildInputs = with pkgs; with pkgs.haskellPackages; [
            actionlint
            cabal-install
            feedback.packages.${system}.default
            ghcid
            haskell-language-server
            hlint
            hpack
            nil
            nixpkgs-fmt
            ormolu
            statix
          ];
          inherit (precommitCheck) shellHook;
        };

        checks = { pre-commit-check = precommitCheck; };

      });
  nixConfig = {
    extra-substituters = [
      "https://opensource.cachix.org"
      "https://cache.garnix.io"
    ];

    extra-trusted-public-keys = [
      "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}

