{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring
      , containers, criterion, ghc-prim, hedgehog, mmorph, mtl
      , reflection, stdenv
      }:
      mkDerivation {
        pname = "hcobs";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring containers ghc-prim reflection
        ];
        testHaskellDepends = [
          base bytestring ghc-prim hedgehog mmorph mtl reflection
        ];
        benchmarkHaskellDepends = [
          base base64-bytestring bytestring criterion ghc-prim reflection
        ];
        homepage = "https://github.com/berdario/hcobs#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
