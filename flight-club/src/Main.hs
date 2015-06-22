module Main where

import Control.Monad (forever)

import FlightClub.Parser

main :: IO ()
main = mainLoop 

type State = ()

exec :: LogElement -> Maybe String
exec = const Nothing

mainLoop :: IO State
mainLoop = 
  do
    line <- readLn
    case exec (parseLog line) of
      Nothing -> mainLoop 
      Just str -> putStrLn str >> mainLoop 
