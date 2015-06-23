module FlightClub.Actuator where

import FlightClub.Parser

consoleCmd :: String -> String
consoleCmd = ("27276,console,"++)

makeResponse :: LogElement -> Maybe String
makeResponse _ = Just $ consoleCmd "serverMessage I recieved a thing!"


