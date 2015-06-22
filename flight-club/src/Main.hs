module Main where

import Control.Monad (forever, void)
import System.IO

import FlightClub.Parser

commandFile = "./servers/command.txt"
logFile = "./servers/log.txt"

main :: IO ()
main = 
  do
    i <- openFile logFile ReadMode
    o <- openFile commandFile AppendMode
    void $ mainLoop i o

type State = ()

exec :: LogElement -> Maybe String
exec = const Nothing

mainLoop :: Handle -> Handle -> IO State
mainLoop i o = 
  do
    line <- hGetLine i
    putStrLn $ "recieved " ++ line
    case exec (parseLog line) of
      Nothing -> mainLoop i o
      Just str -> 
        do
          putStrLn $ "writing " ++ str
          hPutStrLn o str >> mainLoop i o
