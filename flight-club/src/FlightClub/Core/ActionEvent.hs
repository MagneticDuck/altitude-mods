-- * ACTIONS are things that behaviours can execute, creating
-- side-effects on their environment.
-- * EVENTS are stimula that can trigger behaviours.
module FlightClub.Core.ActionEvent (
-- Exports {{{
  -- Misc Nouns
  PlayerID, VaporID, Nick, Player(..)
  , Tourny, ServerState(..)

  -- Action
  -- * Constructors
  , Action(..)
  -- * Accessors
  , commandFromAction

  -- Event 
  -- * Constructors
  , Event(..)
  , eventFromLog
-- }}}
) where

import Text.JSON
import Data.List
import Control.Applicative

-- Misc Nouns {{{
type PlayerID = Int
type VaporID = String
type Nick = String

data Player = Player 
  { getPlayerID :: PlayerID
  , getVaporID :: VaporID
  , getNick :: Nick } deriving (Show, Eq)

type Tourny = Bool

data ServerState = 
  ServerState 
    { getPlayers :: [Player]
    , getTourny :: Bool } deriving (Show, Eq)
-- }}}

-- Action {{{
data Action = 
  MessageAction String -- sends a message for all players to see
  | WhisperAction Nick String -- sends a message for only one player to see
  | AssignAction Nick Int -- assigns a player to a team
  | TournyAssignAction Nick Int -- assigns a player to a team in a tournament
  | TournyAction Bool -- enables or disables the tournament
      deriving (Show, Eq)

commandFromAction :: Action -> String
commandFromAction action =
  ("27276,console," ++) $
    case action of
      MessageAction str -> unwords
        ["serverMessage", show str]
      WhisperAction nick str -> unwords 
        ["serverWhisper", show nick, show str]
      AssignAction nick team -> unwords
        ["assignTeam", show nick, show team]
      TournyAssignAction nick team -> unwords 
        ["modifyTournament", show nick, show team]
      TournyAction bool ->
        case bool of
          True -> "startTournament"
          False -> "stopTournament"
-- }}}

data Event =
  ChatEvent PlayerID String  -- a player says something
  | JoinEvent Player -- a player starts entering the server
  | LeaveEvent Player -- a player leaves the server
  | StatusEvent ServerState -- the server dumps its current status
  | MoveEvent PlayerID Int -- a player moves to a team
  | ClockEvent Float -- an interval of time passes
  | PingEvent -- server sends log of recent latency time results 
  deriving (Show, Eq)

eventFromLog  :: String -> Maybe Event
eventFromLog str = 
  case decode str of
    Ok a -> parseJson a
    Error _ -> Nothing

parseJson :: JSValue -> Maybe Event
parseJson value = 
  case value of
    (JSObject o) -> 
      parseList (fromJSObject o)
    _ -> Nothing

stringFromValue :: JSValue -> Maybe String
stringFromValue val =
  case val of
    JSString str -> Just $ fromJSString str
    _ -> Nothing

intFromValue :: JSValue -> Maybe Int
intFromValue val =
  case val of
    JSRational _ num -> Just $ round num
    _ -> Nothing

boolFromValue :: JSValue -> Maybe Bool
boolFromValue val =
  case val of
    (JSBool x) -> Just x
    _ -> Nothing

listFromValue :: JSValue -> Maybe [JSValue]
listFromValue val =
  case val of
    (JSArray xs) -> Just xs
    _ -> Nothing

objectFromValue :: JSValue -> Maybe (JSObject JSValue)
objectFromValue val =
  case val of
    JSObject obj -> Just obj
    _ -> Nothing

type LogAttrs = [(String, JSValue)]

getAttr :: String -> LogAttrs -> Maybe JSValue
getAttr name attrs =
  case find ((== name) . fst) attrs of
    Just (_, val) -> Just val
    Nothing -> Nothing

attrType :: LogAttrs -> Maybe String
attrType attrs = stringFromValue =<< getAttr "type" attrs

parseList :: LogAttrs -> Maybe Event
parseList attrs = 
  case attrType attrs of
    Just "chat" -> do 
      message <- stringFromValue =<< getAttr "message" attrs
      player <- intFromValue =<< getAttr "player" attrs
      return $ ChatEvent player message
    Just "clientAdd" -> do
      player <- intFromValue =<< getAttr "player" attrs
      vapor <- stringFromValue =<< getAttr "vaporId" attrs
      nick <- stringFromValue =<< getAttr "nickname" attrs
      return $ JoinEvent (Player player vapor nick)
    Just "clientRemove" -> do
      player <- intFromValue =<< getAttr "player" attrs
      vapor <- stringFromValue =<< getAttr "vaporId" attrs
      nick <- stringFromValue =<< getAttr "nickname" attrs
      return $ LeaveEvent (Player player vapor nick)
    Just "logServerStatus" -> do
      players <- mapM intFromValue =<< listFromValue =<< getAttr "playerIds" attrs
      vapors <- mapM stringFromValue =<< listFromValue =<< getAttr "vaporIds" attrs
      nicks <- mapM stringFromValue =<< listFromValue =<< getAttr "nicknames" attrs  
      tourny <- boolFromValue =<< getAttr "tournamentInProgress" attrs
      players <- return $
        flip map (zip3 players vapors nicks) $ (\(player, vapor, nick) ->
          Player player vapor nick)
      return . StatusEvent $
        ServerState players tourny
    Just "teamChange" -> do
      player <- intFromValue =<< getAttr "player" attrs
      team <- intFromValue =<< getAttr "team" attrs
      return $ MoveEvent player team
    Just "pingSummary" -> do
      pings <- fromJSObject <$> 
        (objectFromValue =<< getAttr "pingByPlayer" attrs)
      return PingEvent
    _ -> Nothing
-- }}}
