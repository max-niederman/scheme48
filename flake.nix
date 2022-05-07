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
              vanessa =
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

          flake = pkgs.vanessa.flake { };
        in
        flake // {
          defaultPackage = flake.packages."vanessa:exe:vanessa";

          apps = rec {
            vanessa = {
              type = "app";
              program = "${flake.packages."vanessa:exe:vanessa"}/bin/vanessa";
            };

            default = vanessa;
          };
        }
    );
}
