{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let
  mkLauncherConfig = import ./launcher-config.nix { inherit pkgs; };

  mkMod = { launcherConfig ? null, service ? null }:
    stdenv.mkDerivation {
      name = "mod";
      
      phases = "installPhase";

      installPhase = ''
        mkdir -p $out

        ${lib.optionalString (! isNull launcherConfig) ''
          mkdir -p $out/servers/
          cp ${launcherConfig} $out/servers/launcher_config.xml 
        ''}

        ${lib.optionalString (! isNull service) ''
          cp ${service} $out/run
        ''}
      '';
    };

  flightClub = { mkDerivation, base, stdenv, json }:
    mkDerivation {
      pname = "flight-club";
      version = "0.1.0.0";
      src = ./flight-club;
      isLibrary = false;
      isExecutable = true;
      buildDepends = [ base json ];
      homepage = "magnetic.uk.to";
      license = stdenv.lib.licenses.publicDomain;
    };

  haskellEngine = 
    stdenv.mkDerivation {
      name = "engine"; 
      src = haskellPackages.callPackage flightClub {};
      phases = "installPhase"; installPhase = "cp $src/bin/flight-club $out";
    };

in

{
  null = mkMod { };
 
  default =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          server-name = "magneticDuck's FLIGHT CLUB";
          server-password = "ruleone";
          server-players = "40";
          server-rcon = "snowmanbomb";
          server-ball = true;
        };
      service = haskellEngine;
    };

  simple-tbd =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          server-name = "magnet_ticDuck's Test Server";
          server-players = "16";
          server-rcon = "snowmanbomb";
        };
    };

  inherit haskellEngine;
}
