(import ./default.nix {}).ghc.shellFor
        {
          withHoogle = true;
          packages = hpkgs: with hpkgs; [ agon ];
        }
