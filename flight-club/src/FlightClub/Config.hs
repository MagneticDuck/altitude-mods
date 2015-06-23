data State = 
  State
    { getServerState :: ServerState 
    , getNumber :: Int }

initState :: State
initState = State (ServerState [] False) 0

getResponse :: (State, LogElement) -> (State, [String])
getResponse = applyBehaviour $
  mconcat [commandsBehaviour]

commandsBehaviour :: Behaviour
commandsBehaviour = chatBehaviour (\(state, str) -> 
  case map toLower . unwords . words $ str of
    "!welcome" -> (,) state $
      serverMessages
        [ "welcome to flight club, the place for good altitude" ]
    "!players" -> (,) state $
      serverMessages
        [ show . getServerState $ state ]
    _ -> (state, [])
  )
