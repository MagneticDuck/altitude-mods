-- * A BEHAVIOUR describes the way that a system reacts to EVENTS, mutating
-- their state and creating ACTIONS
module FlightClub.Behaviour (
-- Exports {{{  
  -- Behaviour
  -- * Constructors
  Behaviour(..)
  , simpleB
  , pureB
  , feedB
  , Zoom, nullZoom
  , zoomB
  -- * Accessors
  , runBehaviour
-- }}}
) where

import Data.Char
import System.IO
import System.CPUTime

import FlightClub.ActionEvent

-- Behaviour {{{
-- s: state
-- i: input
newtype Behaviour s i = 
  Behaviour { runB :: (s, i) -> (s, [Action]) }

joinBehaviour :: Behaviour s i -> Behaviour s i -> Behaviour s i
joinBehaviour b1 b2 = 
  Behaviour (\(s, i) -> 
  case runB b1 (s, i) of
    (s1, a1) -> 
      case runB b2 (s1, i) of
        (s2, a2) -> (s2, a1 ++ a2))

nullBehaviour :: Behaviour s i
nullBehaviour = Behaviour (\(s, _) -> (s, []))

simpleB :: (i -> [Action]) -> Behaviour () i
simpleB f = Behaviour (\(s, i) -> (s, f i))

pureB :: ((s, i) -> [Action]) -> Behaviour s i
pureB f = Behaviour (\(s, i) -> (s, f (s, i)))

-- Behaviour == feedBehaviour return
feedB :: (a -> Maybe b) -> Behaviour s b -> Behaviour s a
feedB feeder behaviour = Behaviour (\(s, a) ->
  case feeder a of
    Just b -> runB behaviour (s, b)
    Nothing -> (s, [])
  )


type Zoom a b = ((a -> b), (b -> a -> a))

nullZoom :: Zoom a ()
nullZoom = (const (), flip const)

zoomB :: Zoom a b -> Behaviour b i -> Behaviour a i
zoomB (getter, setter) behaviour = Behaviour (\(s, i) ->
  case runB behaviour (getter s, i) of
    (s1, actions) -> (setter s1 s, actions)
  )

instance Monoid (Behaviour s i) where
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
getTime = fmap ((/ 10^12) . fromIntegral) getCPUTime

getEvent :: Float -> Handle -> IO (Float, Maybe Event)
getEvent last h = do
  empty <- hIsEOF h
  if not empty then 
    fmap (((,) last) . eventFromLog) $ hGetLine h
  else do 
    now <- getTime
    if (now - last) > 1 then 
      return (now, Just $ ClockEvent (now - last)) 
    else getEvent last h

runBehaviour :: s -> Behaviour s Event -> IO ()
runBehaviour i b = do
  clearDebug
  writeDebug $ replicate 50 '*'
  mainLoop 0 b i =<< openLog

-- parameters are: the time at the last clock event,
-- the directing behaviour, the behaviour-realted state,
-- and the log file handle
mainLoop :: Float -> Behaviour s Event -> s -> Handle -> IO ()
mainLoop time b s h = do
  (time1, mevent) <- getEvent time h
  case mevent of
    Just (ClockEvent _) -> return ()
    Just PingEvent -> return ()
    Nothing -> return ()
    _ -> writeDebug $ "<<<" ++ (show mevent)
  case mevent of
    Just event -> 
      case runB b (s, event) of
        (s1, []) -> mainLoop time1 b s1 h 
        (s1, actions) -> 
          let strs = map commandFromAction actions in
          do
            mapM_ writeDebug (map ((">>>"++) . show) actions)
            (mapM_ writeCommand strs) >> mainLoop time1 b s1 h
    Nothing -> mainLoop time1 b s h
-- }}}  
