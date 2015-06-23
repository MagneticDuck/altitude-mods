-- * A BEHAVIOUR describes the way that a system reacts to EVENTS, mutating
-- their state and creating EVENTS
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

-- the altitude server does this weird thing of
-- writing an EOF to the log file; it's actually

-- a static file, not a pipe
-- this function reads a single element from the log
readLog :: Handle -> IO String
readLog h = do
  empty <- hIsEOF h
  if empty then readLog h
    else hGetLine h

-- commands are also not accepted as a pipe, but as
-- a static file

-- this writes a single command to the command file
writeCommand :: String -> IO ()
writeCommand str =
  withFile commandFile AppendMode (flip hPutStrLn str)

clearDebug :: IO ()
clearDebug = writeFile debugFile ""

writeDebug :: String -> IO ()
writeDebug str = 
  withFile debugFile AppendMode (flip hPutStrLn str)

runBehaviour :: a -> Behaviour a -> IO ()
runBehaviour i b = do
  clearDebug
  writeDebug $ replicate 50 '*'
  mainLoop b i =<< openLog

mainLoop :: Behaviour a -> a -> Handle -> IO ()
mainLoop b s h = do
  line <- readLog h
  writeDebug $ "<<<" ++ line
  case eventFromLog line of
    Just event -> 
      case applyBehaviour b (s, event) of
        (s1, []) -> mainLoop b s1 h 
        (s1, actions) -> 
          let strs = map commandFromAction actions in
          do
            mapM_ writeDebug (map (">>>"++) strs)
            (mapM_ writeCommand strs) >> mainLoop b s1 h
    Nothing -> writeDebug "(no parse)" >> mainLoop b s h
-- }}}  
