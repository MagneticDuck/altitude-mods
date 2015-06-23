module Main where

import Control.Monad (forever, void)
import System.IO

import FlightClub.IO -- readLog, writeCommand
import FlightClub.Parser -- parseLogElement
import FlightClub.Behaviour-- makeResponse

main :: IO ()
main = do
  writeDebug "beginning main loop" 
  mainLoop initState =<< openLog

-- an iteration ot the main loop, waits for and 
-- processes one server log entry
mainLoop :: State -> Handle -> IO ()
mainLoop s h = do
  line <- readLog h
  writeDebug $ "<<<" ++ line
  case parseLogElement line of
    Just log -> 
      case getResponse (s, log) of
        (s1, []) -> mainLoop s1 h 
        (s1, strs) -> do
          mapM_ writeDebug (map (">>>"++) strs)
          (mapM_ writeCommand strs) >> mainLoop s1 h
    Nothing -> mainLoop s h
