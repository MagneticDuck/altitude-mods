-- this module defines the top-level makeResponse method
-- along with its state datatype
module FlightClub.Behaviour (
  State(..), initState,
  makeResponse
) where

import FlightClub.Parser
import FlightClub.Actuator

data State = 
  State
    { getServerState :: ServerState }

initState :: State
initState = State $ ServerState [] False

makeResponse :: (State, LogElement) -> (State, [String])
makeResponse (state, log) =
  (,) state $ echoLog log

echoLog :: LogElement -> [String]
echoLog log = 
  serverMessages [show log]

--simpleGlobalResponse :: String -> [String]
--simpleGlobalResponse str =
  --case map toLower . unwords . words $ str of
    --"!welcome" -> 
      --map serverMessage
        --[ "welcome to flight club, the place for good altitude" 
        --, "***********************************************************"]
    --_ -> []
