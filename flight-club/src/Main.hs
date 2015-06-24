module Main where

import Control.Monad (forever, void)
import System.IO
import Data.Maybe

import FlightClub.Behaviour
import FlightClub.ActionEvent

main :: IO ()
main = 
  runBehaviour () $ mconcat [assignSpecB]

assignSpecB :: Behaviour () Event
assignSpecB = Behaviour (\(state, event) ->
  case event of
    ClockEvent _ -> [AssignAction "magne_ticDuck", -1]
  )

getChat :: Event -> Maybe String
getChat event =
  case event of
    ChatEvent _ str -> Just str
    _ -> Nothing

sayB, countdownB, putUpB :: Behaviour (Maybe Int) Event
sayB = Behaviour (\(state, event) ->
  case event of
    ClockEvent _ -> 
      (state, maybeToList $ fmap (MessageAction . show) state)
    _ -> (state, [])
  )
countdownB = Behaviour (\(state, event) ->
  let mutate x = if x > 0 then Just (x - 1) else Nothing 
  in
    case event of
      ClockEvent _ -> (mutate =<< state, [])
      _ -> (state, [])
  )
putUpB = feedBehaviour getChat $ Behaviour (\(state, str) ->
  case str of
    "!countdown" -> (Just 10, [])
    _ -> (state, [])
  )

