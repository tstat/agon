{ runCommand, cabal2nix, lib, haskellLib, fetchFromGitHub }:

self: super: with haskellLib; with builtins; with lib.strings;
let callLocalPkg = name: pth:
    let src' = lib.cleanSourceWith { filter = filt; src = pth; };
        filt = path: type:
        let isHiddenFile = hasPrefix "." (baseNameOf path);
        in !isHiddenFile;
    in self.callCabal2nix name src' {};
in
{
  agon = callLocalPkg "agon" ../.;
}
