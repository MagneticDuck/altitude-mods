module FlightClub.Actuator where

import Data.Char

import FlightClub.Parser

consoleCmd :: String -> String
consoleCmd = ("27276,console,"++)

serverMessage :: String -> String
serverMessage = consoleCmd . ("serverMessage " ++)

data State = State { }

initState = State

makeResponse :: (State, LogElement) -> (State, [String])
makeResponse (state, (ChatLog str)) = 
  (,) state $ 
    simpleGlobalResponse str

simpleGlobalResponse :: String -> [String]
simpleGlobalResponse str =
  case map toLower . unwords . words $ str of
    "!welcome" -> 
      map serverMessage
        [ "welcome to flight club, the place for good altitude"
        , "***************************************************"]
    _ -> []

