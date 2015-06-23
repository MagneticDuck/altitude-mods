-- this module defines the basic IO interface with the altitude
-- server and its files
module FlightClub.IO (
  openLog, clearDebug,
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

openLog :: IO Handle
openLog = openFile logFile ReadMode

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
