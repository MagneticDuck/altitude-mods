module Main where

import Control.Monad (forever, void)
import System.IO

import FlightClub.Behaviour
import FlightClub.ActionEvent

main :: IO ()
main = 
  runBehaviour 1 myBehaviour

myBehaviour :: Behaviour Int
myBehaviour = chatBehaviour (\(state, str) ->
  case str of
    "add" -> (state + 1, [])
    "show" -> (state, [MessageAction $ show state])
    _ -> (state, [])
  )
