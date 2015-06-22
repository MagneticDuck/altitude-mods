{ pkgs ? import <nixpkgs> {}}:
with pkgs;

let
  flightClub = { mkDerivation, base, stdenv }:
    mkDerivation {
      pname = "flight-club";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      buildDepends = [ base ];
      homepage = "magnetic.uk.to";
      license = stdenv.lib.licenses.unfree;
    };

in
  haskellPackages.callPackage flightClub {}
