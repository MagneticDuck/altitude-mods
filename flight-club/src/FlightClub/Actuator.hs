module FlightClub.Actuator where

import Data.Char

import FlightClub.Parser

consoleCmd :: String -> String
consoleCmd = ("27276,console,"++)

serverMessage :: String -> String
serverMessage = consoleCmd . ("serverMessage " ++)

serverMessages :: [String] -> [String]
serverMessages = map serverMessage

data State = State { }

initState = State

makeResponse :: (State, LogElement) -> (State, [String])
makeResponse (state, log) = 
  (,) state $ echoLog log

echoLog :: LogElement -> [String]
echoLog log = 
  case log of
    (ChatLog _ _) -> serverMessages [show log]
    (ClientAdd _ _ _) -> serverMessages [show log]

simpleGlobalResponse :: String -> [String]
simpleGlobalResponse str =
  case map toLower . unwords . words $ str of
    "!welcome" -> 
      map serverMessage
        [ "welcome to flight club, the place for good altitude" 
        , "***********************************************************"]
    _ -> []

