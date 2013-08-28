module Ser2net where

import System.Posix.Types
import System.Posix.Process
import System.Posix.Signals

reloadConfig :: ProcessID -> IO ()
reloadConfig =
  signalProcess sigHUP

restartDaemon :: IO ()
restartDaemon =
  let
    p = executeFile "service" True [ "ser2net", "restart" ] Nothing
  in
    do 
      pid <- forkProcess p
      pStatus <- getProcessStatus True True pid
      case pStatus of
        Just _ -> return ()
        Nothing -> return ()


