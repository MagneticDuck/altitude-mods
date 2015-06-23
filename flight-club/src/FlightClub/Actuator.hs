module FlightClub.Actuator where

import FlightClub.Parser

consoleCmd :: String -> String
consoleCmd = ("27276,console,"++)

makeResponse :: LogElement -> [String]
makeResponse (ChatLog str) = 
  [consoleCmd $ "serverMessage " ++ str]

