module Main where

import Data.Maybe
import Data.Char
import Data.List

import FlightClub.Behaviour
import FlightClub.ActionEvent

data State = State 
  { getServer :: ServerState 
  , getLocked :: Bool
  , getLock :: ([VaporID], [VaporID])} deriving (Show, Eq)

initState :: State
initState = State 
  { getServer = ServerState { getPlayers = [], getTourny = False } 
  , getLocked = False
  , getLock = ([], [])}

serverZoom :: Zoom State ServerState
serverZoom = (getServer, (\x s -> s { getServer = x }))

getCommand :: Event -> Maybe [String]
getCommand event =
  case event of
    ChatEvent _ str ->
      case head str of
        '.' -> Just . map (map toLower) . words $ tail str
        _ -> Nothing
    _ -> Nothing

main :: IO ()
main = 
  runBehaviour initState . mconcat $
    [ feedB getCommand pureCommandsB 
    , feedB getCommand commandsB
    , maintainLockB
    , zoomB serverZoom watchState ]

clearTeams :: State -> [Action]
clearTeams state =
  map (flip AssignAction (-1) . getNick) $ 
    getPlayers . getServer $ state

searchPlayer :: State -> String -> Maybe Player
searchPlayer state str = 
  findPlayer state ((== str) . filter (/= ' ') . map toLower . getNick)

pureCommandsB :: Behaviour State [String]
pureCommandsB = pureB (\(state, cmds) ->
  case head cmds of
    "show" -> [MessageAction (show state)]
    "who" ->
      case tail cmds of
        [searchStr] -> 
          case searchPlayer state searchStr of
            Nothing -> 
              [MessageAction $ "no player found matching " ++ searchStr]
            Just player -> 
              [MessageAction $ "player found: " ++ show player]
    "ping" -> [MessageAction "pong"] 
    _ -> []
  )

commandsB :: Behaviour State [String]
commandsB = Behaviour (\(state, cmds) ->
  case head cmds of
    "clear" -> (state { getLock = ([], []) }, clearTeams state)
    "lock" ->
      case tail cmds of
        ["on"] -> 
          ( state { getLocked = True } 
          , clearTeams state ++ [MessageAction "lock mode is now on!"])
        ["off"] -> 
          ( state { getLocked = False } 
          , [MessageAction "lock mode is now off!"])
        [] -> (,) state . (:[]) . MessageAction $ 
          if (getLocked state) then "lock mode is on" 
            else "lock mode is off"
        _ -> (state, [MessageAction "bad arguments to lock command"])
    _ -> (state, [])
  )

findPlayer :: State -> (Player -> Bool) -> Maybe Player
findPlayer state = flip find players
  where players = getPlayers . getServer $ state

getTeam :: ([VaporID], [VaporID]) -> VaporID -> Int
getTeam (team1, team2) vapor =
  case find (== vapor) team1 of
    Just _ -> 0
    Nothing ->
      case find (== vapor) team2 of
        Just _ -> 1
        Nothing -> (-1)

maintainLockB :: Behaviour State Event
maintainLockB = pureB (\(state, event) ->
  let lock = getLock state in
  case getLocked state of
    True -> 
      case event of 
        MoveEvent id _ -> 
          case findPlayer state ((== id) . getPlayerID) of
            Just player -> (:[]) $
              AssignAction 
                (getNick player) 
                (getTeam lock (getVaporID player))
            _ -> []
        _ -> []
    _ -> []
  )

watchState :: Behaviour ServerState Event
watchState = Behaviour (\(state, event) -> 
  case event of
    StatusEvent new -> (new, [])
    JoinEvent player ->
      let players = getPlayers state in
        (state { getPlayers = player:players }, [])
    LeaveEvent player ->
      let players = getPlayers state in flip (,) [] $
        state 
          { getPlayers = 
              filter ((/= getVaporID player) . getVaporID) players }
    _ -> (state, [])
  )
