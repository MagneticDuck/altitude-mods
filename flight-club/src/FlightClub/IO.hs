module FlightClub.IO (
  readLog, writeCommand, writeDebug
)where

import System.IO

-- the filepaths to the pseudo-pipes created by the server
commandFile, logFile, debugFile :: FilePath
commandFile = "./servers/command.txt" 
logFile = "./servers/log.txt"
debugFile = "./debug"

-- the altitude server does this weird thing of
-- writing an EOF to the log file; it's actually
-- a static file, not a pipe

-- this function reads a single element from the log
readLog :: IO String
readLog = do
  empty <- fmap null $ readFile logFile
  if empty then readLog 
    else do
      contents <- fmap lines $ readFile logFile
      writeFile logFile $ unlines . tail $ contents
      return $ head contents

-- commands are also not accepted as a pipe, but as
-- a static file

-- this writes a single command to the command file
writeCommand :: String -> IO ()
writeCommand str =
  withFile commandFile AppendMode (flip hPutStrLn str)

writeDebug :: String -> IO ()
writeDebug str = 
  withFile debugFile AppendMode (flip hPutStrLn str)
