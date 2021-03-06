module Main where

-- imports {{{
import Data.Maybe
import Data.Char
import Data.List
import System.Environment

import FlightClub.Core
import FlightClub.State
-- }}}

main :: IO ()
main = do
  args <- getArgs
  admins <- 
    case args of 
      [adminFile] -> fmap (map (take 36) . lines) $ readFile adminFile
      _ -> return []
  runBehaviour initState $ mconcat
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

      -- ** help command **
    , helpB

      -- ** online admins command **
    , showAdminB admins

      -- ** all-user command behaviours **
    , feedB getCommand . mconcat $
        [ normalCommandsB -- simple useful commands 
        ]

     -- ** admin-only command behaviours **
    , mconcat
      [ feedB (getAdminCommand admins) adminCommandsB -- admin commands
      , feedB (getEventWhen ((== TournyPlay) . getMode)) $
          feedB (getAdminCommand admins) tournyAdminCommandsB 
          -- some admin commands have extra teeth in a tournament
          -- (assign all players to teams)
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

-- helpB {{{
helpstr :: [String]
helpstr =
  [ "This is the helpfile for the flight club server system."
  , "TEAM ASSIGNMENTS:"
  , ">>The server manages a 'team assignment' state seperately from the actually effective teams currently in vigor on the server."
  , ">>>>.move <player> <team>: moves the player with <player> as a (not case sensitive) infix of their name to <team>, where <team> is left, right, or spec."
  , ">>>>.swap <player> <player>: swaps two players"
  , ">>>>.swapteams: swaps the two teams"
  , ">>>>.teams: displays the current teams for all to see."
  , "SERVER MODES:"
  , ">>The server can be in one of three modes, which are 'free', 'stop', or 'tourny'."
  , ">>>>Free mode: the server acts like a pub server."
  , ">>>>Stop move: no players may spawn."
  , ">>>>Tourny mode: the teams are dictated by the server's team assignments." 
  , "MISC COMMANDS:"
  , ">>.ping: requests that the server make a 'pong' response."
  , ">>.show: debug feature, displays the server state."
  , ">>.wait10: starts a timer that makes the server respond in 10 seconds."]

smallHelpstr :: [String]
smallHelpstr =
  [ "This is the reduced helpfile for the flight club server system."
  , "TEAM ASSIGNMENTS: .move <player> <team>, .swap <player> <player>, .swapteams, .teams"
  , "SERVER MODES: .free, .stop, .tourny" 
  , "MISC COMMANDS: .ping, .show, .wait10" 
  , "BASIC USAGE for captains game: .free, .clear, .move (captains), .show (ask captains if they're ready), .tourny, .move (players), /vote changeMap"]

helpB :: Behaviour State Event
helpB = pureB (\(state, event) ->
  case event of
    ChatEvent id str ->
      case words . map toLower $ str of
        [".help"] ->
          case nickFromID state id of
            Just player -> map (WhisperAction player) helpstr
            _ -> []
        [".h"] ->
          case nickFromID state id of
            Just player -> map (WhisperAction player) smallHelpstr
            _ -> []
        _ -> []
    _ -> []
  )
-- }}}

-- showAdminB {{{
showAdminB :: [String] -> Behaviour State Event
showAdminB admins = pureB (\(state, event) ->
  case event of
    ChatEvent _ str ->
      case words . map toLower $ str of
        [".admin"] ->
          case filter (flip elem admins . getVaporID) (getPlayers . getServer $ state) of
            [] -> [MessageAction "no admins currently online! sorry!"]
            players ->
              [MessageAction $ 
                "admins currently online: " ++ (unwords . map getNick $ players)]
        _ -> []
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

writeTeam :: 
  State -> VaporID -> Int -> State
writeTeam state vid int =
  let
    (teamLeft1, teamRight1) = getTeams state
    removePlayer = filter (/= vid)
    (teamLeft2, teamRight2) = 
      (removePlayer teamLeft1, removePlayer teamRight1)
  in
    state 
      { 
        getTeams =
          case int of
            0 -> (vid:teamLeft2, teamRight2)
            1 -> (teamLeft2, vid:teamRight2)
            _ -> (teamLeft2, teamRight2) 
      }

assignTeamsSoft :: State -> [Action]
assignTeamsSoft state =
  map 
    (\player -> 
        AssignAction 
          (getNick player) 
          (getTeam (getTeams state) (getVaporID player))) 
    (getPlayers . getServer $ state)

assignTeams :: State -> [Action]
assignTeams state =
  map 
    (\player -> 
        TournyAssignAction 
          (getNick player) 
          (getTeam (getTeams state) (getVaporID player))) 
    (getPlayers . getServer $ state)

tournyAdminCommandsB :: Behaviour State [String]
tournyAdminCommandsB = Behaviour (\(state, cmds) ->
  case take 1 cmds of
    ["clear"] -> (state, assignTeams state)
    ["swapteams"] -> (state, assignTeams state)
    _ -> (state, [])
  )

adminCommandsB :: Behaviour State [String]
adminCommandsB = Behaviour (\(state, cmds) ->
  case take 1 cmds of
    ["free"] -> 
      ( state { getMode = FreePlay }
      , [TournyAction False, MessageAction $ describeMode FreePlay] )
    ["stop"] ->
      ( state { getMode = StopPlay } 
      , ((TournyAction False):) $ 
          clearTeams state ++ [MessageAction $ describeMode StopPlay] )
    ["tourny"] ->
      case getTeams state of
        ((_:_), (_:_)) ->
          ( state { getMode = TournyPlay }
          , (++) 
              (assignTeamsSoft state ++ (TournyAction True):(assignTeams state))
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
    ["move"] ->
      case tail cmds of
        [searchStr, teamStr] ->
          case (searchPlayer state searchStr, readTeam teamStr) of
            (Just player, Just team) -> 
              (writeTeam state (getVaporID player) team 
              , 
                if ((== TournyPlay) . getMode) state then
                  [TournyAssignAction (getNick player) team]
                else [] 
              )
            _ -> (state, [MessageAction "bad arguments"])
        _ -> (state, [])
    ["clear"] -> ( state { getTeams = ([],[]) }, [] )
    ["swapteams"] -> 
      let (teamLeft, teamRight) = getTeams state in
        ( state { getTeams = (teamRight, teamLeft) }, [] )
    ["swap"] -> 
      case tail cmds of
        [player1, player2] ->
          case map (searchPlayer state) [player1, player2] of 
            [Just player1, Just player2] ->
              let 
                teams = getTeams state
                team1 = getTeam teams (getVaporID player1)
                team2 = getTeam teams (getVaporID player2)
                actions =
                  if ((== TournyPlay) . getMode) state then
                    [ TournyAssignAction (getNick player1) team2
                    , TournyAssignAction (getNick player2) team1 ]
                  else []
              in
                flip (,) actions $
                  (\x -> writeTeam x (getVaporID player1) team2) $
                    writeTeam state (getVaporID player2) team1 
            _ -> (state, [MessageAction "cannot find player"])
        _ -> (state, [])
    _ -> (state, [])
  )
-- }}}

readTeam :: String -> Maybe Int
readTeam team =
  case team of
    "spec" -> Just (-1)
    "left" -> Just 0
    "right" -> Just 1
    _ -> Nothing

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
