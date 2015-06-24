module FlightClub.State (
  -- * State
  -- ** Constructors
  State(..)
  , initState
  -- ** Accessors
  -- *** Zooms
  , serverZoom
  -- *** Feeders
  , getCommand
  , getAdminCommand
  , getEventWhen
) where

import Data.Char
import Data.List

import FlightClub.Core

data State = State 
  { getServer :: ServerState -- server info
  , getDelayedActions :: [(String, Float, [Action])] -- actions on a time-bomb 
  , getLocked :: Bool -- whether the game is locked (nobody can join)
  , getTeams  :: ([VaporID], [VaporID]) -- teams to be used in tournament
  } deriving (Show, Eq)

initState :: State
initState = State 
  { getServer = ServerState { getPlayers = [], getTourny = False } 
  , getDelayedActions = [] 
  , getLocked = False
  , getTeams = ([],[]) }

serverZoom :: Zoom State ServerState
serverZoom = (getServer, (\x s -> s { getServer = x }))

getCommand :: (State, Event) -> Maybe [String]
getCommand (_, event) =
  case event of
    ChatEvent _ str ->
      case take 1 str of
        "." -> Just . map (map toLower) . words $ tail str
        _ -> Nothing
    _ -> Nothing

adminVapors :: [VaporID]
adminVapors = 
  [ "5640761e-f165-4f40-b3d6-3e3167dd767d" -- magneticDuck
  ]

getAdminCommand :: (State, Event) -> Maybe [String]
getAdminCommand (state, event) =
  let players = getPlayers . getServer $ state in
  case event of
    ChatEvent playerid str ->
      case find ((== playerid) . getPlayerID) players of
        Just player -> 
          if getVaporID player `elem` adminVapors then
            getCommand (state, event)
          else Nothing
        _ -> Nothing
    _ -> Nothing

getEventWhen :: (State -> Bool) -> (State, Event) -> Maybe Event
getEventWhen p (state, event) =
  if p state then Just event else Nothing
