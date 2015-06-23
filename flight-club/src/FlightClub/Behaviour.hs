-- this module defines the top-level makeResponse method
-- along with its state datatype
module FlightClub.Behaviour (
  State(..), initState,
  getResponse 
) where

import Data.Char

import FlightClub.Parser
import FlightClub.Actuator

type Commands = [String]
newtype Behaviour = 
  Behaviour { applyBehaviour :: (State, LogElement) -> (State, Commands) }

joinBehaviour :: Behaviour -> Behaviour -> Behaviour
joinBehaviour b1 b2 = 
  Behaviour (\input@(_, log) -> 
  case applyBehaviour b1 input of
    (state1, commands) -> 
      case applyBehaviour b2 (state1, log) of
        (state2, commands2) -> (state2, commands ++ commands2))

nullBehaviour :: Behaviour
nullBehaviour = Behaviour (\(state, log) -> (state, []))

instance Monoid Behaviour where
  mempty = nullBehaviour
  mappend = joinBehaviour

data State = 
  State
    { getServerState :: ServerState 
    , getNumber :: Int }

initState :: State
initState = State (ServerState [] False) 0

getResponse :: (State, LogElement) -> (State, [String])
getResponse = applyBehaviour $
  commandsBehaviour 

commandsBehaviour :: Behaviour
commandsBehaviour = Behaviour (\(state, log) -> 
  case log of
    ChatLog _ str ->
      case map toLower . unwords . words $ str of
        "!welcome" -> (,) state $
          serverMessages
            [ "welcome to flight club, the place for good altitude" ]
        _ -> (state, [])
    _ -> (state, [])
  )

