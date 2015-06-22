{ pkgs }:

with pkgs;

writeScript "engine" ''
  sleep 50;
  echo '27276,console,serverWhisper here I am' >> servers/command.txt;
''  
