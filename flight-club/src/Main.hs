module Main where

-- imports {{{
import Data.Maybe
import Data.Char
import Data.List

import FlightClub.Core
import FlightClub.State
-- }}}

main :: IO ()
main = runBehaviour initState $ 
  mconcat
    [ 
      -- ** always-on behaviours (related to basic functioning) **
      zoomB serverZoom watchStateB -- keeps track of player list
    , runDelayedB -- runs delayed actions

      -- ** greeting behaviours **
    , addJoiningB -- add joining players to list
    , greetB -- greet joining players when they load into the game

      -- ** stopped behaviours **
    , feedB (getEventWhen ((== StopPlay) . getMode)) . mconcat $
        [ protectStopB ]

      -- ** all-user command behaviours **
    , feedB getCommand . mconcat $
        [ normalCommandsB -- simple useful commands 
        ]

     -- ** admin-only command behaviours **
    , feedB getAdminCommand . mconcat $
        [ adminCommandsB -- admin commands
        ]
    ]

-- watchStateB {{{
watchStateB :: Behaviour ServerState Event
watchStateB = Behaviour (\(state, event) -> 
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
-- }}}

-- runDelayedB {{{
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
-- }}}

-- addJoiningB {{{
addJoiningB :: Behaviour State Event
addJoiningB = Behaviour (\(state, event) ->
  case event of
    JoinEvent player -> 
      let 
        old = getJoining state 
        action = 
          [MessageAction $ getNick player ++ " is joining..."]
      in
        (state { getJoining = player:old }, action)
    _ -> (state, [])
  )
-- }}}

-- greetB {{{
makeGreet :: Nick -> [Action]
makeGreet nick =
  [ MessageAction . unwords $ 
      ["please welcome", nick, "to flight club!"] 
  , WhisperAction nick "**** this server is currently in beta, contact magneticDuck if you have questions or concerns **** " ]

greetB :: Behaviour State Event
greetB = Behaviour (\(state, event) ->
  case event of
    MoveEvent playerid _ ->
      case find ((== playerid) . getPlayerID) (getJoining state) of
        Just player ->
          let 
            newjoining = 
              filter ((/= playerid) . getPlayerID) 
                (getJoining state) in
          (state {getJoining = newjoining},  makeGreet (getNick player))
        Nothing -> (state, [])
    _ -> (state, [])
  )
-- }}}

-- protectLockB {{{
protectStopB :: Behaviour State Event
protectStopB = pureB (\(state, event) ->
  case event of
    MoveEvent player _ ->
      maybeToList $ flip AssignAction (-1) <$> nickFromID state player 
    _ -> []
  )
-- }}}

-- normalCommandsB {{{
normalCommandsB :: Behaviour State [String]
normalCommandsB = 
  let
    pure = pureB (\(state, cmds) ->
      case take 1 cmds of
        ["show"] -> [MessageAction (show state)]
        ["ping"] -> [MessageAction "pong"] 
        _ -> []
      )
  in
    mappend pure $ Behaviour (\(state, cmds) ->
      case take 2 cmds of
        ["wait10"] -> flip (,) [MessageAction "waiting 10 seconds..."] $
          flip addDelayed state $
            ("wait", 10, 
              [MessageAction "10 seconds have passed"])
        _ -> (state, [])
    )
-- }}}

-- adminCommandsB {{{
clearTeams :: State -> [Action]
clearTeams state =
  map (flip AssignAction (-1) . getNick) $ 
    getPlayers . getServer $ state

describeMode :: PlayMode -> String
describeMode mode =
  case mode of
    FreePlay -> "free play: players may join as they wish"
    StopPlay -> "game is stopped: no players will join"
    TournyPlay -> "tournament: players may join their teams"

getTeam :: ([VaporID], [VaporID]) -> VaporID -> Int
getTeam (team1, team2) vapor =
  case find (== vapor) team1 of
    Just _ -> 0
    Nothing ->
      case find (== vapor) team2 of
        Just _ -> 1
        Nothing -> (-1)

adminCommandsB :: Behaviour State [String]
adminCommandsB = Behaviour (\(state, cmds) ->
  case take 1 cmds of
    ["free"] -> 
      ( state { getMode = FreePlay }
      , (TournyAction False):[MessageAction $ describeMode FreePlay] )
    ["stop"] ->
      ( state { getMode = StopPlay } 
      , clearTeams state ++ [MessageAction $ describeMode StopPlay] )
    ["tourny"] ->
      case getTeams state of
        ((_:_), (_:_)) ->
          let 
            assignPlayers =
              map 
                (\player -> 
                    TournyAssignAction 
                      (getNick player) 
                      (getTeam (getTeams state) (getVaporID player))) 
                (getPlayers . getServer $ state)
          in
            ( state { getMode = TournyPlay }
            , (++) 
                ((TournyAction True):assignPlayers)
                [MessageAction $ describeMode TournyPlay])
        _ -> (state, [MessageAction "cannot start tournament with null teams"])
    ["who"] ->
      case tail cmds of
        [searchStr] -> (,) state . (:[]) . MessageAction $ 
          case searchPlayer state searchStr of
            Just player -> getNick player
            Nothing -> "cannot find player"
        _ -> (state, [])
    ["teams"] ->
      let 
        showTeam team = 
          unwords $ concatMap (\x -> maybeToList . fmap getNick . findPlayer state $ (== x) . getVaporID) team
      in (,) state $ 
        [ MessageAction . ("left team: " ++) $ 
            showTeam (fst . getTeams $ state)
        , MessageAction . ("right team: " ++) $ 
            showTeam (snd . getTeams $ state) ]
    --["move"] ->
      --case tail cmds of
        --[searchStr, team] -> 
          --case fuzzyFindPlayer state searchStr of
            --Just player -> 
             --(setTeamLock state player team, setTeamAction player team)
            --Nothing -> (state, [MessageAction "cannot find player"])
    _ -> (state, [])
  )
-- }}}

--setTeamAction :: Player -> String -> [Action]
--setTeamAction player team =
  --case team of
    --"spec" -> [AssignAction (getNick player) (-1)]
    --"left" -> [AssignAction (getNick player) 0]
    --"right" -> [AssignAction (getNick player) 1]
    --_ -> []
--
--setTeamLock :: State -> Player -> String -> State
--setTeamLock state player team =
  --let
    --lock@(team1, team2) = getLock state
    --withoutPlayer@(team1without, team2without) = 
      --(\f -> (f team1, f team2)) $ filter (/= (getVaporID player))
  --in
  --case team of
    --"spec" -> state { getLock = withoutPlayer }
    --"left" -> state { getLock = ((getVaporID player):team1without, team2without) }
    --"right" -> state { getLock = (team1without, (getVaporID player):team2without) }
    --_ -> state
--
--
