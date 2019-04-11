{ compiler ? "ghc864" }:


let
  pkgs = (import ./nix/world.nix { inherit compiler; }).pkgs;
  ghc = pkgs.haskell.packages."${compiler}";
in { agon = ghc.agon;
     agon-exe = pkgs.haskell.lib.justStaticExecutables ghc.agon;
   }
