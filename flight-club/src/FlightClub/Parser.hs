module FlightClub.Parser(
  -- LogElement
  -- Constructors
  LogElement(..)
  , parseLogElement
) where

import Text.JSON
import Data.List
import Control.Applicative

type PlayerID = Int
type VaporID = String
type Nick = String

data LogElement =
  ChatLog PlayerID String 
  | ClientAdd PlayerID VaporID Nick deriving (Show, Eq)

parseLogElement :: String -> Maybe LogElement
parseLogElement str = 
  case decode str of
    Ok a -> parseJson a
    Error _ -> Nothing

parseJson :: JSValue -> Maybe LogElement
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

type LogAttrs = [(String, JSValue)]

getAttr :: String -> LogAttrs -> Maybe JSValue
getAttr name attrs =
  case find ((== name) . fst) attrs of
    Just (_, val) -> Just val
    Nothing -> Nothing

attrType :: LogAttrs -> Maybe String
attrType attrs = 
  case getAttr "type" attrs of
    Just val -> 
      case val of
        JSString str -> Just $ fromJSString str
        _ -> Nothing
    Nothing -> Nothing

parseList :: LogAttrs -> Maybe LogElement
parseList attrs = 
  case attrType attrs of
    Just "chat" -> do 
      message <- stringFromValue =<< getAttr "message" attrs
      player <- intFromValue =<< getAttr "player" attrs
      return $ ChatLog player message
    Just "clientAdd" -> do
      player <- intFromValue =<< getAttr "player" attrs
      vapor <- stringFromValue =<< getAttr "vaporId" attrs
      nick <- stringFromValue =<< getAttr "nickname" attrs
      return $ ClientAdd player vapor nick
    _ -> Nothing
