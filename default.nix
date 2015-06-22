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
          echo "cat servers/log.txt | ${service} >> servers/command.txt" > $out/run
          chmod +x $out/run
        ''}
      '';
    };

  simpleMod = 
    writeScript "simple-mod.sh" 
      ''
        while true; do
          read a;
          echo 27276,console,serverMessage $a
        done
      '';

in

{
  null = mkMod { };
 
  default =
    mkMod {
      launcherConfig =
        mkLauncherConfig {
          server-name = "magne_ticDuck's Flight Club (in development)";
          server-password = "WHforPresident";
          server-players = "40";
          server-rcon = "snowmanbomb";
          server-ball = true;
        };
      service = simpleMod;
    };
}
