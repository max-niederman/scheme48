{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "nixpkgs/nixos-21.11";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs) lib haskell haskellPackages;

          name = "vanessa";

          project = devTools:
            haskellPackages.developPackage {
              root = lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
              inherit name;
              returnShellEnv = devTools != [ ];

              modifier = with haskell.lib.compose;
                (lib.trivial.flip lib.trivial.pipe) [
                  (addBuildTools devTools)
                  dontHaddock
                  enableStaticLibraries
                  justStaticExecutables
                  disableLibraryProfiling
                  disableExecutableProfiling
                ];
            };
        in
        rec {
          packages.vanessa = project [ ];
          defaultPackage = packages.vanessa;

          devShell = project (with haskellPackages; [
            haskell-language-server
            cabal-install
            stylish-haskell
          ]);
        }
      );
}
