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

in

{
  kcoop = run-src kcoop-src;
}
