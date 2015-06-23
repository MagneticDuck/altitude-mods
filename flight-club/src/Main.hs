module Main where

import Control.Monad (forever, void)
import System.IO
import Data.Maybe

import FlightClub.Behaviour
import FlightClub.ActionEvent

main :: IO ()
main = 
  runBehaviour Nothing $ mconcat [sayB, countdownB, putUpB]

sayB, countdownB, putUpB :: Behaviour (Maybe Int)
sayB = Behaviour (\(state, event) ->
  case event of
    ClockEvent _ -> 
      (state, maybeToList $ fmap (MessageAction . show) state)
    _ -> (state, [])
  )
countdownB = Behaviour (\(state, event) ->
  case event of
    ClockEvent _ -> (fmap (subtract 1) state, [])
    _ -> (state, [])
  )
putUpB = chatBehaviour (\(state, str) ->
  case str of
    "!countdown" -> (Just 10, [])
    _ -> (state, [])
  )

