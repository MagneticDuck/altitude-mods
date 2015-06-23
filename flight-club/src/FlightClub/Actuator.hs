-- this module defines methods for constructing commands to be written to 
-- command.txt, exported in the form of [String] from makeResponse
module FlightClub.Actuator where

import Data.Char

import FlightClub.Parser

consoleCmd :: String -> String
consoleCmd = ("27276,console,"++)

serverMessage :: String -> String
serverMessage = consoleCmd . ("serverMessage " ++)

serverMessages :: [String] -> [String]
serverMessages = map serverMessage
