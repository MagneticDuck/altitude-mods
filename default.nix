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

  haskellEngine = 
    stdenv.mkDerivation {
      name = "engine"; 
      src = import ./flight-club/default.nix { inherit pkgs; };
      phases = "installPhase"; installPhase = "cp $src/bin/flight-club $out";
    };

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
      service = haskellEngine;
    };

  inherit haskellEngine;
}
