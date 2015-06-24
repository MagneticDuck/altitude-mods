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

main :: IO ()
main = 
  runBehaviour initState . mconcat $
    [ feedB getCommand commandsB ]

pingB :: Behaviour () [String]
pingB = simpleB (\cmds ->
  case head cmds of
    "ping" -> [MessageAction "pong"] 
    _ -> []
  )

commandsB :: Behaviour State [String]
commandsB = mappend (zoomB nullZoom pingB) $
  pureB (\(state, cmds) ->
    case head cmds of
      "show" -> [MessageAction (show state)]
      _ -> []
  )

manageServer :: Behaviour ServerState Event
manageServer = Behaviour (\(state, event) -> 
  case event of
    StatusEvent new -> (new, [])
    JoinEvent player ->
      let players = getPlayers state in
        (state { getPlayers = player:players }, [])
    _ -> (state, [])
  )

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
