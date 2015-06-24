module Main where

import Data.Maybe
import Data.Char
import Data.List

import FlightClub.Behaviour
import FlightClub.ActionEvent

data State = State 
  { getServer :: ServerState 
  , getLocked :: Bool
  , getDelayedActions :: [(String, Float, [Action])]
  , getLock :: ([VaporID], [VaporID])} deriving (Show, Eq)

initState :: State
initState = State 
  { getServer = ServerState { getPlayers = [], getTourny = False } 
  , getLocked = False
  , getDelayedActions = []
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

adminVapors :: [VaporID]
adminVapors = ["5640761e-f165-4f40-b3d6-3e3167dd767d"]

getAdminCommand :: (State, Event) -> Maybe [String]
getAdminCommand (state, event) =
  let players = getPlayers . getServer $ state in
  case event of
    ChatEvent playerid str ->
      case find ((== playerid) . getPlayerID) players of
        Just player -> 
          if getVaporID player `elem` adminVapors then
            getCommand event
          else Nothing
        _ -> Nothing
    _ -> Nothing

getLockEvent :: (State, Event) -> Maybe Event
getLockEvent (state, event) =
  if getLocked state then Just event else Nothing

main :: IO ()
main = 
  runBehaviour initState . mconcat $
    [ 
      -- zooms state watching behaviour to server
      -- element of the top-level state (keeps track of
      -- player list)
      zoomB serverZoom watchState 

      -- runs delayed actions
    , runDelayedB

      -- greets players who join
    , greetB

      -- sends all commands to debug command behaviour
    , feedB getCommand debugCommandsB 

      -- sends all admin-issued commands to admin command behaviour
    , stateFeedB getAdminCommand adminCommandsB

      -- sends all events that occur during team lock to the 
      -- lock-maintanence behaviour
    , stateFeedB getLockEvent maintainLockB ]

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


runDelayedB :: Behaviour State Event
runDelayedB = Behaviour (\(state, event) ->
  case event of
    ClockEvent x ->
      let
        delayedActions = getDelayedActions state
        finishedActions = 
          concatMap 
            (\(_, delay, action) -> 
                if (delay - x) < 0 then action else []) 
            delayedActions     
        unfinishedActions =
          concatMap
            (\(name, delay, action) ->
                if (delay - x) > 0 then [(name, (delay - x), action)] else [])
          delayedActions
      in
        (state { getDelayedActions = unfinishedActions }, finishedActions)
    _ -> (state, [])
  )

makeGreet :: Nick -> [Action]
makeGreet nick =
    [ MessageAction . unwords $ 
        ["please welcome", nick, "to flight club!"] 
    , WhisperAction nick "**** this server is currently in beta, contact MagneticDuck if you have questions or concerns **** " ]

greetB :: Behaviour State Event
greetB = Behaviour (\(state, event) ->
  case event of
    JoinEvent (Player _ _ nick) ->
      (addDelayed ("greet", 2, makeGreet nick) state, [])
    _ -> (state, [])
  )

addDelayed :: (String, Float, [Action]) -> State -> State
addDelayed d state =
  let delayedActions = getDelayedActions state in
    state { getDelayedActions = d:delayedActions }

removeDelayed :: String -> State -> State
removeDelayed name state =
  let delayedActions = getDelayedActions state in
    state { getDelayedActions = filter (\(x, _, _) -> x /= name) delayedActions }

clearTeams :: State -> [Action]
clearTeams state =
  map (flip AssignAction (-1) . getNick) $ 
    getPlayers . getServer $ state

searchPlayer :: State -> String -> Maybe Player
searchPlayer state str = 
  findPlayer state ((== str) . filter (/= ' ') . map toLower . getNick)

debugCommandsB :: Behaviour State [String]
debugCommandsB = 
  let
    pure = pureB (\(state, cmds) ->
      case head cmds of
        "show" -> [MessageAction (show state)]
        "ping" -> [MessageAction "pong"] 
        _ -> []
      )
  in
    mappend pure $ Behaviour (\(state, cmds) ->
      case head cmds of
        "wait" -> 
          (addDelayed ("wait", 3, [MessageAction "I waited..."]) state, [])
        _ -> (state, [])
    )

adminCommandsB :: Behaviour State [String]
adminCommandsB = Behaviour (\(state, cmds) ->
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
    "who" ->
      case tail cmds of
        [searchStr] -> (,) state . (:[]) . MessageAction $ 
          case fuzzyFindPlayer state searchStr of
            Just player -> getNick player
            Nothing -> "cannot find player"
        _ -> (state, [])
    "move" ->
      case tail cmds of
        [searchStr, team] -> 
          case fuzzyFindPlayer state searchStr of
            Just player -> 
             (setTeamLock state player team, setTeamAction player team)
            Nothing -> (state, [MessageAction "cannot find player"])
    _ -> (state, [])
  )

setTeamAction :: Player -> String -> [Action]
setTeamAction player team =
  case team of
    "spec" -> [AssignAction (getNick player) (-1)]
    "left" -> [AssignAction (getNick player) 0]
    "right" -> [AssignAction (getNick player) 1]

setTeamLock :: State -> Player -> String -> State
setTeamLock state player team =
  let
    lock@(team1, team2) = getLock state
    withoutPlayer@(team1without, team2without) = 
      (\f -> (f team1, f team2)) $ filter (/= (getVaporID player))
  in
  case team of
    "spec" -> state { getLock = withoutPlayer }
    "left" -> state { getLock = ((getVaporID player):team1without, team2without) }
    "right" -> state { getLock = (team1without, (getVaporID player):team2without) }
    _ -> state

findPlayer :: State -> (Player -> Bool) -> Maybe Player
findPlayer state = flip find players
  where players = getPlayers . getServer $ state

fuzzyFindPlayer :: State -> String -> Maybe Player
fuzzyFindPlayer state str =
  findPlayer state (\player ->
    isInfixOf (curate str) (curate . getNick $ player))
  where
    curate = map toLower . filter (`notElem` " _")

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
  case event of 
    MoveEvent id _ -> 
      case findPlayer state ((== id) . getPlayerID) of
        Just player -> (:[]) $ 
          AssignAction 
            (getNick player) 
            (getTeam (getLock state) (getVaporID player))
        _ -> []
    _ -> []
  )

