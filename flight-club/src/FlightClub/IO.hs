module FlightClub.IO (
  openLog,
  readLog, writeCommand
)where

import System.IO

-- the filepaths to the pseudo-pipes created by the server
commandFile, logFile :: FilePath
commandFile = "./servers/command.txt" 
logFile = "./servers/log.txt"

-- the altitude server does this weird thing of
-- writing an EOF to the log file; it's actually
-- a static file, not a pipe

openLog :: IO Handle
openLog = openFile logFile ReadMode

-- this function reads a single element from the log
readLog :: Handle -> IO String
readLog h = do
  blocked <- hIsEOF h
  if blocked then readLog h
    else hGetLine h

-- commands are also not accepted as a pipe, but as
-- a static file

-- this writes a single command to the command file
writeCommand :: String -> IO ()
writeCommand str =
  withFile commandFile AppendMode (flip hPutStrLn str)
