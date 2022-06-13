{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring
      , criterion, deepseq, hedgehog, mmorph, mtl
      , reflection, stdenv, weigh, lib
      }:
      mkDerivation {
        pname = "hcobs";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring reflection
        ];
        testHaskellDepends = [
          base base64-bytestring bytestring deepseq hedgehog mmorph
          mtl reflection weigh
        ];
        benchmarkHaskellDepends = [
          base base64-bytestring bytestring criterion reflection
        ];
        homepage = "https://github.com/berdario/hcobs#readme";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
