module Main where

import Control.Monad (forever, void)
import System.IO

import FlightClub.IO -- readLog, writeCommand
import FlightClub.Parser -- parseLogElement
import FlightClub.Actuator -- makeResponse

main :: IO ()
main = do
  writeDebug "beginning main loop" 
  mainLoop 

-- an iteration ot the main loop, waits for and 
-- processes one server log entry
mainLoop :: IO ()
mainLoop = do
  line <- readLog 
  writeDebug $ "recieved: " ++ line
  case parseLogElement line of
    Just log -> 
      case makeResponse log of
        [] -> mainLoop 
        strs -> do
          writeDebug "responding: " 
          mapM_ writeDebug (map (">>"++) strs)
          (mapM_ writeCommand strs) >> mainLoop
    Nothing -> writeDebug "log appears to be malformed" >> mainLoop
