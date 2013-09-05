module Ser2net where

import System.Posix.Types
import System.Posix.Process
import System.Posix.Signals

data Action = StartDaemon | StopDaemon | RestartDaemon deriving ( Eq, Show )

reloadConfig :: ProcessID -> IO ()
reloadConfig =
  signalProcess sigHUP

restartDaemon :: IO ()
restartDaemon = manageDaemon RestartDaemon

stopDaemon :: IO ()
stopDaemon = manageDaemon StopDaemon

manageDaemon :: Action ->  IO ()
manageDaemon a =
  let
    action = case a of
               StartDaemon -> "start"
               StopDaemon -> "stop"
               RestartDaemon -> "restart"

    p = executeFile "service" True [ "ser2net", action ] Nothing
  in
    do 
      pid <- forkProcess p
      pStatus <- getProcessStatus True True pid
      case pStatus of
        Just _ -> return ()
        Nothing -> return ()


