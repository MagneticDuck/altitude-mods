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

  jonusArrowTbd =
    fetchurl {
      name = "tbd_arrow_new.altx";
      url = "http://magnetic.uk.to/misc/maps/tbd_arrow.altx";
      sha256 = "0kqk8d8yb9d9n1wvcy9ij53mlvmcmxpxgskwrh6smgrs3bs2v73l";
    };

  jonusArrowTbd2 =
    fetchurl {
      name = "tbd_arrow2.altx";
      url = "http://magnetic.uk.to/misc/maps/tbd_arrow2.altx";
      sha256 = "1ldsfqmkf4k4bsfbnckl6lwac8vqip6bxpplr5g26s4vlkij9vfa";
    };

  jonusArrowBall =
    fetchurl {
      name = "ball_arrow.altx";
      url = "http://magnetic.uk.to/misc/maps/ball_arrow.altx";
      sha256 = "1ad0pg0mqq4ql39i7wl243wq9l3d44mkdzq4w7mdz9vdgxrdavjp";
    };

  tbgLobby =
    fetchurl {
      name = "ball_tbd.altx";
      url = "http://www.dropbox.com/s/7yj6ubq7vlwxxo8/ball_tbg.altx?dl=0";
      sha256 = "0dwyvvjdnyhh66jfzsz208mm3iyndzxbs47l2zvc0ghmvj8fhim2";

    };

  flightClub = { mkDerivation, base, stdenv, json }:
    mkDerivation {
      pname = "flight-club"; version = "0.1.0.0";
      src = ./flight-club;
      isLibrary = false; isExecutable = true;
      buildDepends = [ base json ];
      license = stdenv.lib.licenses.publicDomain;
    };

  haskellService = admins:
    writeScript "flight-club-service" ''
      ${haskellPackages.callPackage flightClub {}}/bin/flight-club ${admins}
    '';  

  admins = 
    [ "5640761e-f165-4f40-b3d6-3e3167dd767d" # duck
      "faa8061c-5cd6-43bb-b389-561be0792b33" # kafka
      "55dd4345-53e2-4a5d-b6e1-7ca357c7337d" # label
      "7c29079d-5ead-4136-8b1d-467513350b79" # demv
      "b61aa791-7cc3-44bf-bba0-1c7faa9009a9" # toma 
      "31456ecc-b5f2-4615-88b4-c4aeadcbf0a7" # golden
      "bf60335d-784f-4f31-98f8-3e0bb591b8c2" # moxy
      "8612de03-f66c-4281-9d7b-8b751837a3c6" # jonus
      "2f2101b2-bfda-4f95-8f6d-a590992d8108" # max
      "92f40cad-d09f-48b4-b165-5d3c3d97f26c" # brutal
      "0204199e-fee0-47fc-9c22-efcbbdedc9af" # fluffy
      "02a33655-b2eb-42c2-bc88-fff24240f0d8" # phyx
      "0aa6b54c-52b4-4fa7-b2b4-0dc0c8350dd0" # paiza
      "d2ade87e-09a6-4ff9-af7b-9ae9f58fd570" # elusive
      "f4d0b170-2877-4a92-90eb-eb950a57c636" # stam
    ];

  tbgAdmins = 
    [ "5640761e-f165-4f40-b3d6-3e3167dd767d" # duck
      "f4d0b170-2877-4a92-90eb-eb950a57c636" # stam
      "25c28e39-8c93-4adb-91c0-783cb9d75959" # smile
    ];

  adminFile = customAdmins:
    writeTextFile {
      name = "adminfile"; text = lib.concatStringsSep "\n" customAdmins;
    };
  
in

{
  null = mkMod { };
 
  default =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          name = "FLIGHT CLUB";
          port = "27276";
          password = "ruleone";
          players = "40";
          lobby = "lobby_club";
          maps = ["|tbd|" "|1dm|" "|ball|" "|1de|" "|tdm|" "tbd_arrow" "ball_arrow" "tbd_arrow2"];
          admins = admins;
        };
      service = (haskellService (adminFile admins));
      extraMaps = [
        {src = mangoLobby; name = "lobby_club.altx";}
        {src = jonusArrowBall; name = "ball_arrow.altx";}
        {src = jonusArrowTbd; name = "tbd_arrow.altx";}
        {src = jonusArrowTbd2; name = "tbd_arrow2.altx";}];
    };
  
  tbg =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          name = "{TBG} Official Training Server";
          port = "27277";
          password = "{TBG}";
          players = "40";
          lobby = "ball_tbg";
          maps = ["|tbd|" "|1dm|" "|ball|" "|1de|" "|tdm|" "tbd_arrow" "ball_arrow" "tbd_arrow2"];
          admins = tbgAdmins;
        };
      service = (haskellService (adminFile tbgAdmins));
      extraMaps = [
        {src = tbgLobby; name = "ball_tbg.altx";}
        {src = jonusArrowBall; name = "ball_arrow.altx";}
        {src = jonusArrowTbd; name = "tbd_arrow.altx";}
        {src = jonusArrowTbd2; name = "tbd_arrow2.altx";}];
    };

  inherit haskellEngine;
}
