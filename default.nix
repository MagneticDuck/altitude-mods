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

  tbdRabbit = 
    fetchurl {
      name = "tbd_rabbit.altx";
      url = "https://www.dropbox.com/s/yuh78tf45sjq4tl/tbd_rabbit.altx?dl=0";
      sha256 = "0ms5ydaj73nvavvfsm2p0cdxvj64qjlfqi21c45axrcsmixhyppq";
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
      name = "lobby_tbg.altx";
      url = "https://www.dropbox.com/s/ns9e4f9hzgq3sun/lobby_tbg.altx?dl=0";
      sha256 = "1cxbl7cwdn29k91hihhc96v4xzsp292vajca42w1p1irjv3xjqmq";
    };
    
  ballAntre =
    fetchurl {
      name = "ball_antre_pb.altx";
      url = "https://www.dropbox.com/sh/b10kan7xis6bcpr/AADAxxMbiUzNdOxwEs37SvpIa/1/ball_antre_pb.altx?dl=0";
      sha256 = "1vpa0lymcrk7zr8kyri5n5bmrf3akd9lmng5sj7dlbbp5aa01fk7";
    };
    
  ballCave =
    fetchurl {
      name = "ball_cave_pb.altx";
      url = "https://www.dropbox.com/sh/b10kan7xis6bcpr/AADAxxMbiUzNdOxwEs37SvpIa/1/ball_cave_pb.altx?dl=0";
      sha256 = "1wq45wpcnz8874976br8mgbjayxh6v58ijh2d73nh6qfwxida2vw";
    };
    
  ballIce =
    fetchurl {
      name = "ball_ice_pb.altx";
      url = "https://www.dropbox.com/sh/b10kan7xis6bcpr/AADAxxMbiUzNdOxwEs37SvpIa/1/ball_ice_pb.altx?dl=0";
      sha256 = "0sjjyg30c8v97853hhqac14dxsrpc692nwsbha7rvj8i27bbz41l";
    };
    
   ballLostcity2 =
    fetchurl {
      name = "ball_lostcity2_pb.altx";
      url = "https://www.dropbox.com/sh/b10kan7xis6bcpr/AADAxxMbiUzNdOxwEs37SvpIa/1/ball_lostcity2_pb.altx?dl=0";
      sha256 = "1pki222sxy173m4skq7s8ykxjgbg50vmkqw8xw2ikagpln8nfigr";
    };   
    
  ballSnow =
    fetchurl {
      name = "ball_snow_pb.altx";
      url = "https://www.dropbox.com/sh/b10kan7xis6bcpr/AADAxxMbiUzNdOxwEs37SvpIa/1/ball_snow_pb.altx?dl=0";
      sha256 = "0j3dgbs1igya7d4g2pk0mz37vs5dammx9z6blw6kmy1nqn8s0mq1";
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
      "02a33655-b2eb-42c2-bc88-fff24240f0d8" # phyx
      "0aa6b54c-52b4-4fa7-b2b4-0dc0c8350dd0" # paiza
      "d2ade87e-09a6-4ff9-af7b-9ae9f58fd570" # elusive
      "f4d0b170-2877-4a92-90eb-eb950a57c636" # stam
      "5ec76fe2-7074-4926-8873-a0e2bf2d9ba5" # dmcm
      "ba9e19db-2cec-4c01-b984-14ca9313f9ff" # rawr
    ];

  tbgAdmins = 
    [ "5640761e-f165-4f40-b3d6-3e3167dd767d" # duck
      "f4d0b170-2877-4a92-90eb-eb950a57c636" # stam
      "2cc35ae9-dd99-413b-bbd3-2e1a4ac5b024" # ring
      "c9e24c41-292d-4d40-b76c-230d8f30ef32"
      "e9ff8bb7-ca22-4cc6-b45b-187db4697e9c"
      "2ac67d3a-3103-4443-9fb4-1d72219ddb88"
    ];

  adminFile = customAdmins:
    writeTextFile {
      name = "adminfile"; text = lib.concatStringsSep "\n" customAdmins;
    };

  biellMaps = fetchFromGitHub {
    owner = "biell";
    repo = "alti-maps";
    rev = "c45cd6d30925e1c09dc3826d1b1e0aa3df4c3bca";
    sha256 = "1y1yabmkrkkaf4jzsfhzdvxicg1hi3zq1y04jjlmwxhlhl1wgww4";
  };

  getBiellMap = mapName:
    runCommand "biell_map" {maps = biellMaps;} ''
      cp $maps/maps/${mapName} $out
    '';
  
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
          password = "discord";
          rcon = "ChickenSun123";
          players = "40";
          lobby = "lobby_tbg";
          maps = ["|tbd|" "|1dm|" "|ball|" "|1de|" "|tdm|" "tbd_arrow" "ball_arrow" "tbd_arrow2" "ball_race_asteroids" "ball_race_eastern_creek" "tbd_rabbit"];
          admins = tbgAdmins;
        };
      extraMaps = [
        {src = tbdRabbit; name = "tbd_rabbit.altx";}
        {src = tbgLobby; name = "lobby_tbg.altx";}
        {src = jonusArrowBall; name = "ball_arrow.altx";}
        {src = jonusArrowTbd; name = "tbd_arrow.altx";}
        {src = jonusArrowTbd2; name = "tbd_arrow2.altx";}
        {src = getBiellMap "ball_race_asteroids.altx"; name = "ball_race_asteroids.altx";}
        {src = getBiellMap "ball_race_eastern_creek.altx"; name = "ball_race_eastern_creek.altx";}
        
        {src = ballAntre; name = "ball_antre_pb.altx";}
        {src = ballCave; name = "ball_cave_pb.altx";}
        {src = ballIce; name = "ball_ice_pb.altx";}
        {src = ballLostcity2; name = "ball_lostcity2_pb.altx";}
        {src = ballSnow; name = "ball_snow_pb.altx";}    
      ];
    };

  inherit haskellEngine;
}
