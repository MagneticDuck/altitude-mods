module Main where

import Data.Maybe

import FlightClub.Behaviour
import FlightClub.ActionEvent

main :: IO ()
main = 
  runBehaviour () $ mconcat [assignSpecB, feedBehaviour getChat pingB]

assignSpecB :: Behaviour () Event
assignSpecB = Behaviour (\(state, event) ->
  case event of
    MoveEvent _ _ -> (state, [AssignAction "magne_ticDuck" (-1)])
    _ -> (state, [])
  )

getChat :: Event -> Maybe String
getChat event =
  case event of
    ChatEvent _ str -> Just str
    _ -> Nothing

pingB :: Behaviour () String
pingB = statelessBehaviour (\chat ->
  case chat of
    "!ping" -> [MessageAction "pong!"]
    _ -> []
  )

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

