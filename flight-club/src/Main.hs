module Main where

import Control.Monad (forever, void)
import System.IO

import FlightClub.Parser -- parseLogElement
import FlightClub.Actuator -- makeResponse

-- the filepaths to the pseudo-pipes created by the server
commandFile, logFile :: FilePath
commandFile = "./servers/command.txt" 
logFile = "./servers/log.txt"

main :: IO ()
main = 
  do
    putStrLn "opening handles to server pseudo-pipes"
    i <- openFile logFile ReadMode
    o <- openFile commandFile AppendMode
    void $ mainLoop i o

-- the altitude server does this weird thing of
-- writing an EOF to the log file; it's actually
-- a static file, not a pipe
waitEvent :: Handle -> IO String
waitEvent h =
  do
    blocked <- hIsEOF h
    if blocked then waitEvent h
      else hGetLine h

-- an iteration ot the main loop, waits for and 
-- processes one server log entry
mainLoop :: Handle -> Handle -> IO ()
mainLoop i o = 
  do
    line <- waitEvent i
    putStrLn $ "recieved " ++ line
    case makeResponse (parseLogElement line) of
      Nothing -> mainLoop i o
      Just str -> 
        do
          putStrLn $ "writing " ++ str
          hPutStrLn o str >> mainLoop i o
