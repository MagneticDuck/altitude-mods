module FlightClub.Parser(
  -- LogElement
  -- Constructors
  LogElement(..)
  , parseLogElement
) where

import Text.JSON
import Data.List

data LogElement =
  ChatLog String

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
    Just "chat" -> 
      case getAttr "message" attrs of
        Just str -> fmap ChatLog (stringFromValue str)
        Nothing -> Nothing
    _ -> Nothing
