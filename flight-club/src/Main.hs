module Main where

import Control.Monad (forever, void)
import System.IO

import FlightClub.Behaviour

main :: IO ()
main = 
  runBehaviour () mempty
