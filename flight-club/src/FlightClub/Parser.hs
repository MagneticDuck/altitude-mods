module FlightClub.Parser(
  -- LogElement
  -- Constructors
  LogElement(..)
  , parseLogElement
) where

data LogElement =
  ChatLog String String
  | MiscLog

parseLogElement :: String -> LogElement
parseLogElement str = MiscLog
