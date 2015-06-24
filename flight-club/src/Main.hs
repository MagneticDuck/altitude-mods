module Main where

import Data.Maybe
import Data.Char

import FlightClub.Behaviour
import FlightClub.ActionEvent

data State = State 
  { getServer :: ServerState 
  , getLocked :: Bool } deriving (Show, Eq)

initState :: State
initState = State 
  { getServer = ServerState { getPlayers = [], getTourny = False } 
  , getLocked = False }

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
    , zoomB serverZoom watchState ]


pureCommandsB :: Behaviour State [String]
pureCommandsB = pureB (\(state, cmds) ->
  case head cmds of
    "show" -> [MessageAction (show state)]
    "ping" -> [MessageAction "pong"] 
    "clear" -> clearTeams state
    _ -> []
  )

clearTeams :: State -> [Action]
clearTeams state =
  map (flip AssignAction (-1) . getNick) $ 
    getPlayers . getServer $ state

commandsB :: Behaviour State [String]
commandsB = Behaviour (\(state, cmds) ->
  case head cmds of
    "lock" ->
      case tail cmds of
        ["on"] -> 
          ( state { getLocked = False } 
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

