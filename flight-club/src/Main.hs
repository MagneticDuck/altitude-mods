module Main where

import Control.Monad (forever, void)
import System.IO

import FlightClub.IO -- readLog, writeCommand
import FlightClub.Parser -- parseLogElement
import FlightClub.Actuator -- makeResponse

main :: IO ()
main = do
  putStrLn "beginning main loop" 
  mainLoop =<< openLog

-- an iteration ot the main loop, waits for and 
-- processes one server log entry
mainLoop :: Handle -> IO ()
mainLoop h = do
  line <- readLog h
  putStrLn $ "recieved: " ++ line
  case parseLogElement line of
    Just log -> 
      case makeResponse log of
        [] -> mainLoop h
        strs -> do
          putStrLn "responding: " 
          mapM_ putStrLn (map (">>"++) strs)
          (mapM_ writeCommand strs) >> mainLoop h
    Nothing -> putStrLn "log appears to be malformed" >> mainLoop h
