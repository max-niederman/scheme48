{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [
            haskell-nix.overlay
            (self: super: {
              scheme48 =
                self.haskell-nix.project'
                  {
                    src = ./.;

                    compiler-nix-name = "ghc8107";

                    # development shell
                    shell = {
                      # haskell tools
                      tools = {
                        cabal = { };
                        haskell-language-server = { };
                        hindent = { };
                      };

                      # non-haskell tools
                      buildInputs = with pkgs; [
                        stylish-haskell
                        nixpkgs-fmt
                      ];
                    };
                  };
            })
          ];

          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskell-nix) config;
          };
          inherit (pkgs) lib;

          flake = pkgs.scheme48.flake { };
        in
        flake // {
          defaultPackage = flake.packages."scheme48:exe:scheme48";

          apps = rec {
            scheme48 = {
              type = "app";
              program = "${flake.packages."scheme48:exe:scheme48"}/bin/scheme48";
            };

            default = scheme48;
          };
        }
    );
}
