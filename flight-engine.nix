{ pkgs }:

with pkgs;

writeScript "engine" ''
  while 1; do
    read $a;
    for i in $(echo $a | grep chat); do
      echo '27276,console,serverMessage stfu';
    done;
  done;
''  
