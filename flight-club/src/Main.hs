module Main where

import Data.Maybe
import Data.Char

import FlightClub.Behaviour
import FlightClub.ActionEvent

data State = State 
  { getServer :: ServerState } deriving (Show, Eq)

initState :: State
initState = State 
  { getServer = ServerState { getPlayers = [], getTourny = False } }

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
    [ feedB getCommand commandsB 
    , zoomB serverZoom watchState ]

commandsB :: Behaviour State [String]
commandsB = pureB (\(state, cmds) ->
  case head cmds of
    "show" -> [MessageAction (show state)]
    "ping" -> [MessageAction "pong"] 
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

