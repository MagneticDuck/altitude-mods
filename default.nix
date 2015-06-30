{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let
  mkLauncherConfig = import ./launcher-config.nix { inherit pkgs; };

  mkMod = { launcherConfig ? null, extraMaps ? null, service ? null }:
    stdenv.mkDerivation {
      name = "mod";
      
      phases = "installPhase";

      installPhase = ''
        mkdir -p $out

        ${lib.optionalString (! isNull launcherConfig) ''
          mkdir -p $out/servers/
          cp ${launcherConfig} $out/servers/launcher_config.xml 
        ''}

        mkdir -p $out/maps/
        ${lib.concatMapStrings (map:"cp ${map.src} $out/maps/${map.name}\n") extraMaps}

        ${lib.optionalString (! isNull service) ''
          cp ${service} $out/run
        ''}
      '';
    };

  mangoLobby = 
    fetchurl {
      name = "tbd_lobby.altx";
      url = "http://altitudegame.com/map/mapDownload?m=4d63a8cb-26b5-45a8-b478-6a47aaa7270c";
      sha256 = "1h03ra2wi26v8k2j8sjbhhc6grgb9l4ykfxcqr9frby3pgl52ngs";
    };

  flightClub = { mkDerivation, base, stdenv, json }:
    mkDerivation {
      pname = "flight-club"; version = "0.1.0.0";
      src = ./flight-club;
      isLibrary = false; isExecutable = true;
      buildDepends = [ base json ];
      license = stdenv.lib.licenses.publicDomain;
    };

  haskellService = 
    stdenv.mkDerivation {
      name = "flight-club-service"; 
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
          name = "magneticDuck's FLIGHT CLUB";
          port = "27276";
          password = "ruleone";
          players = "40";
          rcon = "snowmanbomb";
          lobby = "lobby_club";
          maps = ["|premium|tbd|", "lobby_club"];
        };
      service = haskellService ;
      extraMaps = [{src = mangoLobby; name = "lobby_club.altx";}];
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
