{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64-bytestring, bytestring
      , containers, criterion, deepseq, ghc-prim, hedgehog, mmorph, mtl
      , reflection, stdenv, weigh, lib
      }:
      mkDerivation {
        pname = "hcobs";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring containers ghc-prim reflection
        ];
        testHaskellDepends = [
          base base64-bytestring bytestring deepseq ghc-prim hedgehog mmorph
          mtl reflection weigh
        ];
        benchmarkHaskellDepends = [
          base base64-bytestring bytestring criterion ghc-prim reflection
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
