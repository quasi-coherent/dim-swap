{ compiler ? "ghc865"
, nixpkgs ? import (import ./nix/sources.nix).nixpkgs {}
}:

let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  hsPkgs = nixpkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "dim-swap" =
        self.callCabal2nix
          "dim-swap"
          (gitignore ./.)
          {};
    };
  };

  shell = hsPkgs.shellFor {
    packages = ps: [
      ps."dim-swap"
    ];

    buildInputs = with nixpkgs.haskellPackages; [
      ghcid
      hlint
      hsPkgs.cabal-install
      stylish-haskell
    ];

    withHoogle = true;
  };

  exe = nixpkgs.haskell.lib.justStaticExecutables (hsPkgs."dim-swap");

in { inherit shell; inherit exe; }
