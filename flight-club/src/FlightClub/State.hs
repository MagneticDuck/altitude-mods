module FlightClub.State (
  -- * State
  -- ** Constructors
  State(..)
  , initState
  , addDelayed
  , removeDelayed

  -- ** Accessors
  , nickFromID
  , findPlayer
  , searchPlayer

  -- *** Zooms
  , serverZoom

  -- *** Feeders
  , getCommand
  , getAdminCommand
  , getEventWhen
) where

import Data.Char
import Data.List
import Control.Applicative

import FlightClub.Core

data State = State 
  { getServer :: ServerState -- server info
  , getJoining :: [Player]
  , getDelayedActions :: [(String, Float, [Action])] 
      -- actions on a time-bomb 
  , getLocked :: Bool 
      -- whether the game is locked (preventing people from playing)
  , getTeams  :: ([VaporID], [VaporID]) -- teams to be used in tournament
  } deriving (Show, Eq)

initState :: State
initState = State 
  { getServer = ServerState { getPlayers = [], getTourny = False } 
  , getJoining = []
  , getDelayedActions = [] 
  , getLocked = False
  , getTeams = ([],[]) }

addDelayed :: (String, Float, [Action]) -> State -> State
addDelayed d state =
  let delayedActions = getDelayedActions state in
    state { getDelayedActions = d:delayedActions }

removeDelayed :: String -> State -> State
removeDelayed name state =
  let delayedActions = getDelayedActions state in
    state { getDelayedActions = filter (\(x, _, _) -> x /= name) delayedActions }

nickFromID :: State -> Int -> Maybe String
nickFromID state id = getNick <$> 
  findPlayer state ((== id) . getPlayerID) 

findPlayer :: State -> (Player -> Bool) -> Maybe Player
findPlayer state = flip find players
  where players = getPlayers . getServer $ state

searchPlayer :: State -> String -> Maybe Player
searchPlayer state str = 
  findPlayer state ((== str) . filter (/= ' ') . map toLower . getNick)

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

