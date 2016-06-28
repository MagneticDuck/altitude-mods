{ pkgs }:
with pkgs;

{ name, password ? "", players ? "14", port ? "27276",
  admins ? [], maps ? ["|tbd|"], rcon ? "", lobby ? null }:

with pkgs;
    
writeTextFile {
  name = "launcherConfig";
  text = ''
<?xml version="1.0" encoding="UTF-8"?>
<ServerLauncherConfig ip="" upnpEnabled="true" updatePort="27275">
  <servers>
    <AltitudeServerConfig port="${port}" downloadMaxKilobytesPerSecond="500" downloadHttpSource="" serverName="${name}" maxPlayerCount="${players}" hardcore="true" autoBalanceTeams="false" preventTeamSwitching="false" disableBalanceTeamsPopup="true" lanServer="false" callEndOfRoundVote="false" disallowDemoUsers="false" rconEnabled="true" rconPassword="${rcon}" maxPing="1000" minLevel="0" maxLevel="0" secretCode="${password}" cameraViewScalePercent="100">
      <adminsByVaporID>
        ${lib.concatStringsSep "\n" (map (uuid: ''
          <UUID UUID="${uuid}" />
        '') admins)}
      </adminsByVaporID>
      <mapList>
        ${lib.concatStringsSep "\n" (map (map: ''
          <String value="${map}" />
        '') ([lobby] ++ maps))}
      </mapList>
      <mapRotationList>
        ${lib.optionalString (! isNull lobby) ''
          <String value="${lobby}" />
        ''}
      </mapRotationList>
      <BotConfig numberOfBots="0" botDifficulty="MEDIUM" botsBalanceTeams="false" botSpectateThreshold="6" />
      <FreeForAllGameMode scoreLimit="0" RoundLimit="1" roundTimeSeconds="420" warmupTimeSeconds="10" />
      <BaseDestroyGameMode RoundLimit="1" roundTimeSeconds="0" warmupTimeSeconds="10" />
      <ObjectiveGameMode gamesPerRound="9" gamesPerSwitchSides="2" gameWinMargin="1" betweenGameTimeSeconds="6" RoundLimit="1" roundTimeSeconds="0" warmupTimeSeconds="10" />
      <PlaneBallGameMode goalsPerRound="11" RoundLimit="1" roundTimeSeconds="0" warmupTimeSeconds="10" />
      <TeamDeathmatchGameMode scoreLimit="0" RoundLimit="1" roundTimeSeconds="600" warmupTimeSeconds="10" />
      <customCommands />
      <consoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="balanceTeams">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="changeMap">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Vote" />
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="custom">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="drop">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="kick">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="startTournament">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="stopTournament">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="addBan">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="addChatBlock">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="assignTeam">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="ban">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="castBallot">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="chatBlock">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="listBans">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="listChatBlocks">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="listMaps">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="listPlayers">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="logPlanePositions">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="logServerStatus">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="modifyTournament">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="nextMap">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="overrideBallScore">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="overrideSpawnPoint">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="rconPassword">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
            <AltitudeConsoleGroup Group="Anonymous" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="removeBan">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="removeChatBlock">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="serverMessage">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="serverRequestPlayerChangeServer">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="serverWhisper">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="testCameraViewScale">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="testDisableWeaponMode">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="testDS">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="testEM">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="testGravityMode">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="testHealthModifier">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="testPlaneScale">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
        <AltitudeServerConsoleCommandPermissions ConsoleCommand="vote">
          <AllowedGroups>
            <AltitudeConsoleGroup Group="Administrator" />
            <AltitudeConsoleGroup Group="Anonymous" />
          </AllowedGroups>
        </AltitudeServerConsoleCommandPermissions>
      </consoleCommandPermissions>
    </AltitudeServerConfig>
  </servers>
</ServerLauncherConfig>
  '';
}
