module FlightClub.Parser(
  -- LogElement
  -- Constructors
  LogElement(..)
  , parseLog
) where

data LogElement =
  ChatLog String String
  | MiscLog

parseLog :: String -> LogElement
parseLog str = MiscLog
