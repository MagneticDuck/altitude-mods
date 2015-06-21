{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  kcoop-src = stdenv.mkDerivation {
    name = "kcoop";
    src = 
      fetchurl {
        url = "http://bukva-yo.ru/kcoop-0.5.1.zip";
        sha256 = "0ylgs69igvbkl02b6v8zmlpw2ddi5pk4w46675ask4cr53fg61l5";
      };

    phases = "unpackPhase installPhase";

    unpackPhase = ''
      ${unzip}/bin/unzip $src
    '';

    installPhase = ''
      mkdir -p $out
      cp custom_json_commands.txt $out/
      echo "cd kcoop-0.5.1; ./kcoop" > $out/run
      chmod +x $out/run
      cp -r kcoop-0.5.1/ $out/
    '';
  };

  run-src = original : stdenv.mkDerivation {
    name = "run" + original.name;
    src = original;

    phases = "installPhase";

    installPhase = ''
      echo "cp -r $src/* ./" > $out
      chmod +rwx $out
    '';
  };

  mkLauncherConfig = { server-name, server-password ? "", server-players ? "14", server-bots ? "0"}:
    writeTextFile {
      name = "launcherConfig";
      text = ''
<ServerLauncherConfig ip="" upnpEnabled="true" updatePort="27275">
<servers>
  <AltitudeServerConfig port="27276" downloadMaxKilobytesPerSecond="40" downloadHttpSource="" serverName="${server-name}" maxPlayerCount="${server-players}" hardcore="true" autoBalanceTeams="true" preventTeamSwitching="false" disableBalanceTeamsPopup="false" lanServer="false" callEndOfRoundVote="true" disallowDemoUsers="false" rconEnabled="true" rconPassword="" maxPing="" minLevel="0" maxLevel="0" secretCode="${server-password}" cameraViewScalePercent="100">
    <adminsByVaporID />
    <mapList />
    <mapRotationList>
    <String value="|tbd|" />
    </mapRotationList>
    <BotConfig numberOfBots="${server-bots}" botDifficulty="MEDIUM" botsBalanceTeams="true" botSpectateThreshold="6" />
    <BaseDestroyGameMode RoundLimit="1" roundTimeSeconds="0" warmupTimeSeconds="10" />
    <customCommands />
    <consoleCommandPermissions />
  </AltitudeServerConfig>
</servers>
</ServerLauncherConfig>
      '';
    };

  mkMod = { launcherConfig }:
    stdenv.mkDerivation {
      name = "mod";
      
      phases = "installPhase";

      installPhase = ''
        mkdir -p $out/servers/
        cp ${launcherConfig} $out/servers/launcher_config.xml 
      '';
    };

in

{
  simple-tbd = 
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          server-name = "MagneticDuck's Bombs";
          server-bots = "2";
        };
    };

  password-tbd =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          server-name = "MagneticDuck's Hideout";
          server-password = "WHforPresident";
          server-players = "40";
        };
    };
}
