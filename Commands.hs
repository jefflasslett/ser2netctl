module Commands ( validCommand
                , Cmnd( .. )
                , mapStringToCmnd
                , execCommand
                ) where

import Control.Monad
import System.IO
import System.Posix.Types
import Data.Char
import Data.Maybe
import Safe

import qualified System.IO.Strict as S

import Text.Regex.PCRE

import Config
import qualified Options as O
import qualified Ser2net as S2N

data Cmnd = Add | Remove | Update | Show | Restart | Shutdown deriving ( Show, Eq )

mapStringToCmnd :: String -> Maybe Cmnd
mapStringToCmnd s =
  let
    string_cmnd_map = [ ( "add"      , Add )
                      , ( "remove"   , Remove )
                      , ( "restart"  , Restart )
                      , ( "show"     , Show )
                      , ( "shutdown" , Shutdown )
                      , ( "update"   , Update )
                      ]
    lower_s = map toLower s
  in
    lookup lower_s string_cmnd_map

validCommand :: String -> Bool
validCommand str =
  isJust $ mapStringToCmnd str

getDaemonPid :: O.Options -> IO ProcessID
getDaemonPid opts =
  let
    pid = O.optPid opts
  in
    if pid > 0
      then
        return pid
      else
        do
          pid_str <- readFile ( O.optPidFile opts )
          case readMay pid_str of
            Just p -> return p
            Nothing -> return 0

execCommand :: Cmnd -> O.Options -> IO ()
execCommand Add opts      = execAdd opts
execCommand Remove opts   = execRemove opts
execCommand Restart opts  = execRestart opts
execCommand Show opts     = execShow opts
-- execCommand Start opts    = execStart opts
-- execCommand Stop opts     = execStop opts
execCommand Shutdown opts = execShutdown opts
execCommand Update opts   = execUpdate opts


execAdd :: O.Options -> IO ()
execAdd opts =
  let
    pattern = "^" ++ O.optPort opts ++ ":"
    confFilename = O.optConfig opts
  in
    if null $ O.optPort opts
      then
        hPutStrLn stderr "No port specified.  Use -p to specify port to add"
      else
        do
          guts <- S.readFile ( O.optConfig opts )
          if guts =~ pattern :: Bool
            then 
              hPutStrLn stderr "Port already added  Perhaps you want update"
            else
              do
                appendFile confFilename ( makeConfigLine opts )
                pid <- getDaemonPid opts
                when ( O.optReload opts ) ( S2N.reloadConfig pid )
                when ( O.optForce opts ) S2N.restartDaemon


execRemove :: O.Options -> IO ()
execRemove opts =
  let
    pattern = "^" ++ O.optPort opts ++ ":"
    confFilename = O.optConfig opts
  in
    if null $ O.optPort opts
      then
        hPutStrLn stderr "No port specified.  Use -p to specify port to add"
      else
        do
          guts <- S.readFile ( O.optConfig opts )
          let
            ls = lines guts
            filtered = filter ( \l -> not $ l =~ pattern :: Bool ) ls

          writeFile confFilename ( unlines filtered )
          pid <- getDaemonPid opts
          when ( O.optReload opts ) ( S2N.reloadConfig pid )
          when ( O.optForce opts ) S2N.restartDaemon


execRestart :: O.Options -> IO ()
execRestart opts = S2N.restartDaemon

{-
 -execStart :: O.Options -> IO ()
 -execStart opts =
 -  undefined
 -
 -execStop :: O.Options -> IO ()
 -execStop opts =
 -  undefined
 -
 -}

execShow :: O.Options -> IO ()
execShow opts =
  let
    port_pattern = "\\d\\+:\\w\\+:\\d\\+:/dev/\\w\\+"
    confFilename = O.optConfig opts
  in
    do
      guts <- S.readFile confFilename
      let
        ls = lines guts
        portConfigLines = filter ( \l -> l =~ port_pattern :: Bool ) ls
      mapM_ putStrLn portConfigLines


execShutdown :: O.Options -> IO ()
execShutdown opts = S2N.stopDaemon

execUpdate :: O.Options -> IO ()
execUpdate opts =
  let
    confFilename = O.optConfig opts

    update :: O.Options -> String -> String
    update cmdLnOpts l =
      let
        lineOpts = parseConfigLine l
      in
        case lineOpts of
          Nothing -> l
          Just o' -> if O.optPort o' == O.optPort opts
                      then
                        makeConfigLine ( O.mergeOptions o' cmdLnOpts )
                      else
                        l
  in
    if null $ O.optPort opts
      then
        hPutStrLn stderr "No port specified.  Use -p to specify port to update"
      else
        do
          guts <- S.readFile confFilename
          let
            ls = lines guts
            ls' = map ( update opts ) ls

          writeFile confFilename ( unlines ls' )
          pid <- getDaemonPid opts
          when ( O.optReload opts ) ( S2N.reloadConfig pid )
          when ( O.optForce opts ) S2N.restartDaemon




