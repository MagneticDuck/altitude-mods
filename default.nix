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
      url = "http://altitudegame.com/map/mapDownload?m=682c46f3-581f-4b9c-bbc2-08e20c7dbe45";
      sha256 = "0v2zmr9kyjh79jqmd0f42nr5rcprnp3hz4nn3a8qkc8f090xgw09";
    };
    
  tbgsummerlobby =
    fetchurl {
    	name = "lobby_tbgsummer.altx";
    	url = "http://altitudegame.com/map/mapDownload?m=20d1c2c6-9f57-43f6-962c-b294b969ad99";
    	sha256 = "1nq5w40k1zbkh2fxyfr6w4l9bi9jnbanrqvf37ww9mk68zk1xajn";
    };
    
  ballAntre =
    fetchurl {
      name = "ball_antre_pb.altx";
      url = "http://magnetic.uk.to/misc/maps/ball_antre_pb.altx";
      sha256 = "1vpa0lymcrk7zr8kyri5n5bmrf3akd9lmng5sj7dlbbp5aa01fk7";
    };
    
  ballCave =
    fetchurl {
      name = "ball_cave_pb.altx";
      url = "http://magnetic.uk.to/misc/maps/ball_cave_pb.altx";
      sha256 = "03zsnw9diqqagq1pmcsc1m86zk99yilhlmqh03hhk1kwv79if8ls";
    };
    
  ballIce =
    fetchurl {
      name = "ball_ice_pb.altx";
      url = "http://magnetic.uk.to/misc/maps/ball_ice_pb.altx";
      sha256 = "1ny224z5alp1p8h1yqs2yn78k3k66wp5ffs0kp84zwfsdk63wml7";
    };
    
  ballLostcity2 =
    fetchurl {
      name = "ball_lostcity2_pb.altx";
      url = "http://magnetic.uk.to/misc/maps/ball_lostcity2_pb.altx";
      sha256 = "0fmila3kdkmkr1j4sf7qzl4d41kad148x7qxqw3lrb756bnf3h7g";
    };
    
  ballSnow =
    fetchurl {
      name = "ball_snow_pb.altx";
      url = "http://magnetic.uk.to/misc/maps/ball_snow_pb.altx";
      sha256 = "0cg6v1gkmm891jgss1sdp3a59j4nk804ji29ndcap03qvzbnb6sa";
    };
	
  onelhroids =
  fetchurl {
    name = "1lh_asteroids.altx";
    url = "http://altitudegame.com/map/mapDownload?m=a1393da7-3494-423e-bcb4-9344fbcb668c";
    sha256 = "0i8xf2g0p8sqvpn201z21bscaf0nhfk8famxc6w1qnwjzbhz13cf";
  };

  onelhroids2 = 
    fetchurl {
    name = "1lh_asteroids2.altx";
    url = "http://altitudegame.com/map/mapDownload?m=40471826-1bd7-4873-85f1-17065fd2cd08";
    sha256 = "0njvhc8hxdq847mmyj2zgam7r2ivqvcnpvr1x7a8a64aq4i4if9f";
  };

  onelhcave =
    fetchurl {
    name = "1lh_cave.altx";
    url = "http://altitudegame.com/map/mapDownload?m=97dd6ded-7ab1-4071-b79a-4d3f615afd83";
    sha256 = "1axnkrz7ayj2wcsk6l82w9yzf0w8m9w0jbhv45nj0009ar75d6kn";
  };

  onelhcave2 =
    fetchurl {
    name = "1lh_cave2.altx";
    url = "http://altitudegame.com/map/mapDownload?m=04aa4818-f94a-4e7c-84d6-037281188007";
    sha256 = "0hxv5zd7ys0y9ikccbh1wp197l1h4yghgml0j1n6npaam2b9rk6p";
  };

  onelhclocktower =
    fetchurl {
    name = "1lh_clocktower.altx";
    url = "http://altitudegame.com/map/mapDownload?m=bf755e2f-e7d8-44b1-9bb0-48e3a74a9be1";
    sha256 = "1sskhyinj1xpid9xi0v8gaqbjdpx35qmzzis01a136bhj4hn2gr0";
  };

  onelhcore =
    fetchurl {
    name = "1lh_core.altx";
    url = "http://altitudegame.com/map/mapDownload?m=032720b6-460d-40ba-8145-99c5fcabd6af";
    sha256 = "0k41wac2jg0dz2vclxcz45p1rwvalgzhslrriqiamwban3gvvgvy";
  };

  onelhfallout =
    fetchurl {
    name = "1lh_fallout.altx";
    url = "http://altitudegame.com/map/mapDownload?m=68b3da8c-8129-4ba5-ae95-7ee2124f99fc";
    sha256 = "16vm7s8ldf1lnlbhxb34mqxs5b7p0s5sdd6rdg80nbnv42fz2w78";
  };

  onelhgrotto =
    fetchurl {
    name = "1lh_grotto.altx";
    url = "http://altitudegame.com/map/mapDownload?m=9379137f-29fd-49bb-bd23-b725d6d02661";
    sha256 = "19if412yy54z6sr3mrydb7vzdwxfv9ba5jib96y9b4n776sl2jsm";
  };

  onelhlocomotion =
    fetchurl {
    name = "1lh_locomotion.altx";
    url = "http://altitudegame.com/map/mapDownload?m=32dc5076-c0d3-4761-908b-3fc2a29d5895";
    sha256 = "1jn9lxsczj5snkqwk0h7v508w5vzgq67vmbj0nkdcc73zg9p5mpp";
  };

  onelhmayhem =
    fetchurl {
    name = "1lh_mayhem.altx";
    url = "http://altitudegame.com/map/mapDownload?m=7dab829f-c85b-415d-9b57-c17d3b3c125a";
    sha256 = "1l3hyq4miqjqbnyn597w5983mr2jcv8rs7rkbpqrwrk8lcp18xfj";
  };

  onelhmayhem2 =
    fetchurl {
    name = "1lh_mayhem2.altx";
    url = "http://altitudegame.com/map/mapDownload?m=a970db5d-4009-41ef-9f93-11631d1cba0b";
    sha256 = "15jcrhm6ard5aqnvkzvy0vcq70m55v4hxciy1yj6c0h8cx70qwa0";
  };

  onelhmaze =
    fetchurl {
    name = "1lh_maze.altx";
    url = "http://altitudegame.com/map/mapDownload?m=cf39a712-61e8-4a65-ac00-b2825a7aee03";
    sha256 = "1wr1i66150q0wcymsmfq90hwrd4y6mm3shf2bn6mzv72ssfmlbaw";
  };

  onelhmetropolis =
    fetchurl {
    name = "1lh_metropolis.altx";
    url = "http://altitudegame.com/map/mapDownload?m=ee74f4aa-a13d-4b75-8b6f-653a6e04b78a";
    sha256 = "189qpraqsmjb6wbrkkf61sfy8dvgxijpd8gphasxsqjqhysy0n32";
  };

  onelhmiddleground =
    fetchurl {
    name = "1lh_middleground.altx";
    url = "http://altitudegame.com/map/mapDownload?m=cf6e49a4-4645-4920-b075-7c1d2dc999ff";
    sha256 = "0k72jfmv71yyslvq1bkvdwxfy2l0211ddpdsp9r7zjv82gc79wi3";
  };

  onelhplanepark =
    fetchurl {
    name = "1lh_planepark.altx";
    url = "http://altitudegame.com/map/mapDownload?m=1f5bdc36-d2de-4c73-9884-3a0fd5d0e1b5";
    sha256 = "1j6754jgwls8v7nbi04a4p61sxcbx3hp364w5ffscxwhdz1j6vq2";
  };

  onelhslick =
    fetchurl {
    name = "1lh_slick.altx";
    url = "http://altitudegame.com/map/mapDownload?m=e65afeda-4795-4846-9fe7-401158072ee2";
    sha256 = "09514j2klydw9976kiggsyvma1r646rpa4sj7j1wahqc1a0wkz7w";
  };

  onelhtwisted =
    fetchurl {
    name = "1lh_twisted.altx";
    url = "http://altitudegame.com/map/mapDownload?m=fbc3c202-47b3-445d-8b0b-5388854ee81a";
    sha256 = "1wq1rg0yhj5yd47frhnf1hs69fvyjzhh0m7z58pgjs8rgqcn9vnh";
  };

  onelhunderpark =
    fetchurl {
    name = "1lh_underpark.altx";
    url = "http://altitudegame.com/map/mapDownload?m=b4b4a0aa-86bb-4e0f-b76b-50fa440825cb";
    sha256 = "14pa6wlgbd0riv7ivls9kz6m04i3hdbk0x8g0b0pal8kva3wgqxs";
  };

  onelhwoods =
    fetchurl {
    name = "1lh_woods.altx";
    url = "http://altitudegame.com/map/mapDownload?m=11245122-14b1-4a21-8c20-722f1328fe03";
    sha256 = "040m1q8fiqvm1v1vd6m966vkfwwyxal0j591ydl6kj3736dhi86z";
  };

  one1hcrystalized =
    fetchurl {
    	name = "1lh_crystalized.altx";
    	url = "http://altitudegame.com/map/mapDownload?m=9572344d-4726-48e3-9de0-6fed31ba70fb";
    	sha256 = "0x7fm4c4vajbprjxa7r0h6vsx7a2nx5rk0mi6mw55i4xmrrxk8vr";
    };
	
  balltron =
    fetchurl {
	  name = "ball_tron.altx";
	  url = "http://altitudegame.com/map/mapDownload?m=bc3e62f1-075e-48b7-83a3-fa997da88708";
	  sha256 = "1p28gdnn6w4cm0k3j8lbqgfiq2c36v23c40nig239fd2cayy3s1a";
	};
	
  lobbyrock =
    fetchurl {
    	name = "ball_lobbyohairina.altx";
    	url = "https://cdn.discordapp.com/attachments/129240341537751040/183571070589206529/ball_lobbyohairina.altx";
    	sha256 = "1d9795gyjnwd0kabdj5v0i2hh5fl29h3vih8byia4x4ai1yrp6a7";
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
      "f4d0b170-2877-4a92-90eb-eb950a57c636" # stam
      "c9e24c41-292d-4d40-b76c-230d8f30ef32" # stam2
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
      "5ec76fe2-7074-4926-8873-a0e2bf2d9ba5" # dmcm
      "ba9e19db-2cec-4c01-b984-14ca9313f9ff" # rawr
      "0d754ee7-0a53-4bbf-bb9f-1c7e88ca0ee9" # tolis
      "25c28e39-8c93-4adb-91c0-783cb9d75959" # nick
    ];

  tbgAdmins = 
    [ "5640761e-f165-4f40-b3d6-3e3167dd767d" # duck
      "f4d0b170-2877-4a92-90eb-eb950a57c636" # stam
      "c9e24c41-292d-4d40-b76c-230d8f30ef32" # stam2
      "2cc35ae9-dd99-413b-bbd3-2e1a4ac5b024" # ring
      "e9ff8bb7-ca22-4cc6-b45b-187db4697e9c" # mani to ba
      "2ac67d3a-3103-4443-9fb4-1d72219ddb88" # nuffhuff
      "25c28e39-8c93-4adb-91c0-783cb9d75959" # nick
      "0d754ee7-0a53-4bbf-bb9f-1c7e88ca0ee9" # tolis
      "a549e9e0-da2a-44d7-8ad2-e06eeb092e07" # prey
      "325e5779-33e8-4889-9d54-bcda1585630c" # mandel
      "0aa6b54c-52b4-4fa7-b2b4-0dc0c8350dd0" # paiza
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

  onelhMaps = [
        {src = onelhroids; name = "1lh_asteroids.altx";}
        {src = onelhroids2; name = "1lh_asteroids2.altx";}
        {src = onelhcave; name = "1lh_cave.altx";}
        {src = onelhcave2; name = "1lh_cave2.altx";}
        {src = onelhclocktower; name = "1lh_clocktower.altx";}
        {src = onelhcore; name = "1lh_core.altx";}
        {src = onelhfallout; name = "1lh_fallout.altx";}
        {src = onelhgrotto; name = "1lh_grotto.altx";}
        {src = onelhlocomotion; name = "1lh_locomotion.altx";}
        {src = onelhmayhem; name = "1lh_mayhem.altx";}
        {src = onelhmayhem2; name = "1lh_mayhem2.altx";}
        {src = onelhmaze; name = "1lh_maze";}
        {src = onelhmetropolis; name = "1lh_metropolis.altx";}
        {src = onelhmiddleground; name = "1lh_middleground";}
        {src = onelhplanepark; name = "1lh_planepark.altx";}
        {src = onelhslick; name = "1lh_slick.altx";}
        {src = onelhtwisted; name = "1lh_twisted.altx";}
        {src = onelhunderpark; name = "1lh_underpark.altx";}
        {src = onelhwoods; name = "1lh_woods.altx";}
        {src = one1hcrystalized; name = "1lh_crystalized.altx";}
      ];

  
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
          maps = ["|tbd|" "|1dm|" "|ball|" "|1de|" "|tdm|" "tbd_arrow" "ball_arrow" "tbd_arrow2" "1lh_asteroids" "1lh_asteroids2" "1lh_cave" "1lh_cave2" 
				  "1lh_clocktower" "1lh_core" "1lh_fallout" "1lh_grotto" "1lh_locomotion" "1lh_mayhem" "1lh_mayhem2" "1lh_maze" "1lh_metropolis" 
				  "1lh_middleground" "1lh_planepark" "1lh_slick" "1lh_twisted" "1lh_underpark" "1lh_woods"];
          admins = admins;
        };
      service = (haskellService (adminFile admins));
      extraMaps = [
        {src = mangoLobby; name = "lobby_club.altx";}
        {src = jonusArrowBall; name = "ball_arrow.altx";}
        {src = jonusArrowTbd; name = "tbd_arrow.altx";}
        {src = jonusArrowTbd2; name = "tbd_arrow2.altx";}] ++ onelhMaps;
    };
  
  tbg =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          name = "{TBG} Official Training Server";
          port = "27277";
          password = "bunnycarrot";
          rcon = "chickensun123";
          players = "40";
          lobby = "lobby_tbgsummer";
          maps = ["|1dm|" "|ball|" "|1de|" "|tdm|" "ball_arrow" "ball_race_asteroids" "ball_race_eastern_creek" "ball_antre_pb" "ball_cave_pb"
                  "ball_ice_pb" "ball_lostcity2_pb" "ball_snow_pb" "1lh_asteroids" "1lh_asteroids2" "1lh_cave" "1lh_cave2" "1lh_clocktower" 
				  "1lh_core" "1lh_fallout" "1lh_grotto" "1lh_locomotion" "1lh_mayhem" "1lh_mayhem2" "1lh_maze" "1lh_metropolis" 
				  "1lh_middleground" "1lh_planepark" "1lh_slick" "1lh_twisted" "1lh_underpark" "1lh_woods" "1lh_crystalized" "ball_tron"
				  "lobby_tbg" "ball_lobbyohairina"];
          admins = tbgAdmins;
        };
      extraMaps = [
      	{src = tbgsummerlobby; name = "lobby_tbgsummer.altx";}
        {src = tbgLobby; name = "lobby_tbg.altx";}
        {src = jonusArrowBall; name = "ball_arrow.altx";}
        {src = getBiellMap "ball_race_asteroids.altx"; name = "ball_race_asteroids.altx";}
        {src = getBiellMap "ball_race_eastern_creek.altx"; name = "ball_race_eastern_creek.altx";}
        {src = ballAntre; name = "ball_antre_pb.altx";}
        {src = ballCave; name = "ball_cave_pb.altx";}
        {src = ballIce; name = "ball_ice_pb.altx";}
        {src = ballLostcity2; name = "ball_lostcity2_pb.altx";}
        {src = ballSnow; name = "ball_snow_pb.altx";}
        {src = balltron; name = "ball_tron.altx";}
        {src = lobbyrock; name = "ball_lobbyohairina.altx";}] ++ onelhMaps;
    };

  inherit haskellEngine;
}
