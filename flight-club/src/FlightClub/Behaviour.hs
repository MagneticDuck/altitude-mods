-- * A BEHAVIOUR describes the way that a system reacts to EVENTS, mutating
-- their state and creating ACTIONS
module FlightClub.Behaviour (
-- Exports {{{  
  -- Behaviour
  -- * Constructors
  Behaviour(..)
  , chatBehaviour
  -- * Accessors
  , runBehaviour
-- }}}
) where

import Data.Char
import System.IO
import System.CPUTime

import FlightClub.ActionEvent

-- Behaviour {{{
newtype Behaviour a = 
  Behaviour { applyBehaviour :: (a, Event) -> (a, [Action]) }

joinBehaviour :: Behaviour a -> Behaviour a -> Behaviour a
joinBehaviour b1 b2 = 
  Behaviour (\input@(_, log) -> 
  case applyBehaviour b1 input of
    (state1, commands) -> 
      case applyBehaviour b2 (state1, log) of
        (state2, commands2) -> (state2, commands ++ commands2))

nullBehaviour :: Behaviour a
nullBehaviour = Behaviour (\(state, log) -> (state, []))

chatBehaviour :: ((a, String) -> (a, [Action])) -> Behaviour a
chatBehaviour f = Behaviour (\(state, log) ->
  case log of
    ChatEvent _ actions -> f (state, actions)
    _ -> (state, [])
  )

instance Monoid (Behaviour a) where
  mempty = nullBehaviour
  mappend = joinBehaviour
-- }}}

-- Behaviour Accessors {{{
-- the filepaths to the pseudo-pipes created by the server
commandFile, logFile, debugFile :: FilePath
commandFile = "./servers/command.txt" 
logFile = "./servers/log.txt"
debugFile = "./debug"

openLog :: IO Handle
openLog = openFile logFile ReadMode

writeCommand :: String -> IO ()
writeCommand str =
  withFile commandFile AppendMode (flip hPutStrLn str)

clearDebug :: IO ()
clearDebug = writeFile debugFile ""

writeDebug :: String -> IO ()
writeDebug str = 
  withFile debugFile AppendMode (flip hPutStrLn str)

getTime :: IO Float
getTime = fmap ((/ 10^10) . fromIntegral) getCPUTime

getEvent :: Float -> Handle -> IO (Float, Maybe Event)
getEvent last h = do
  now <- getTime
  if (now - last) > 1 
    then return (now, Just $ ClockEvent (now - last)) else do
      empty <- hIsEOF h
      if empty then getEvent last h
        else fmap (((,) last) . eventFromLog) $ hGetLine h

runBehaviour :: a -> Behaviour a -> IO ()
runBehaviour i b = do
  clearDebug
  writeDebug $ replicate 50 '*'
  mainLoop 0 b i =<< openLog

-- parameters are: the time at the last clock event,
-- the directing behaviour, the behaviour-realted state,
-- and the log file handle
mainLoop :: Float -> Behaviour a -> a -> Handle -> IO ()
mainLoop time b s h = do
  (time1, mevent) <- getEvent time h
  writeDebug $ "<<<" ++ (show mevent)
  case mevent of
    Just event -> 
      case applyBehaviour b (s, event) of
        (s1, []) -> mainLoop time1 b s1 h 
        (s1, actions) -> 
          let strs = map commandFromAction actions in
          do
            mapM_ writeDebug (map ((">>>"++) . show) actions)
            (mapM_ writeCommand strs) >> mainLoop time1 b s1 h
    Nothing -> writeDebug "(no parse)" >> mainLoop time1 b s h
-- }}}  
