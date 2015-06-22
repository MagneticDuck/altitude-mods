{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  mkLauncherConfig = { server-name, server-password ? "", server-players ? "14", server-bots ? "0", server-rcon ? "", server-ball ? false }:
    writeTextFile {
      name = "launcherConfig";
      text = ''
<ServerLauncherConfig ip="" upnpEnabled="true" updatePort="27275">
<servers>
  <AltitudeServerConfig port="27276" downloadMaxKilobytesPerSecond="40" downloadHttpSource="" serverName="${server-name}" maxPlayerCount="${server-players}" hardcore="true" autoBalanceTeams="true" preventTeamSwitching="false" disableBalanceTeamsPopup="false" lanServer="false" callEndOfRoundVote="true" disallowDemoUsers="false" rconEnabled="true" rconPassword="${server-rcon}" maxPing="1000" minLevel="0" maxLevel="0" secretCode="${server-password}" cameraViewScalePercent="100">
    <adminsByVaporID />
    <mapList />
    <mapRotationList>
      <String value="|tbd|" />
      ${lib.optionalString server-ball "<String value=\"|ball|\" />"}
    </mapRotationList>
    <BotConfig numberOfBots="${server-bots}" botDifficulty="BRUTAL" botsBalanceTeams="true" botSpectateThreshold="6" />
    <BaseDestroyGameMode RoundLimit="1" roundTimeSeconds="0" warmupTimeSeconds="10" />
    <PlaneBallGameMode goalsPerRound="6" RoundLimit="1" roundTimeSeconds="0" warmupTimeSeconds="10" />
    <customCommands />
    <consoleCommandPermissions />
  </AltitudeServerConfig>
</servers>
</ServerLauncherConfig>
      '';
    };

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
        ''}
      '';
    };

in

{
  null = mkMod { };
 
  default =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          server-name = "MagneticDuck's Hideout";
          server-password = "WHforPresident";
          server-players = "40";
          server-rcon = "snowmanbomb";
          server-ball = true;
        };
    };
}
