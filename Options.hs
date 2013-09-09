{-# LANGUAGE DeriveDataTypeable #-}
module Options ( Options( .. )
               , defaultOptions
               , optionDescriptions
               , ArgumentException( .. )
               , usage_info
               , mergeOptions
               ) where

import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Maybe
import System.Console.GetOpt
import System.Exit
import System.Posix.Files
import System.Posix.Types

import Safe

import qualified PortMode as PM
import qualified BaudRate as BR
import qualified StopBits as SB
import qualified DataBits as DB
import qualified Parity as PA

version :: String
version = "0.1.0.0"

{- These optional arguments could easily be Strings.  GetOpt handles their
 - optional nature just fine.  I like having them as Maybe values though.
 -}
data Options = Options { optPort         :: String
                       , optTty          :: Maybe String
                       , optTimeout      :: Maybe Int
                       , optMode         :: Maybe PM.PortMode
                       , optBaud         :: Maybe BR.BaudRate
                       , optDatabits     :: Maybe DB.DataBits
                       , optStopBits     :: Maybe SB.StopBits
                       , optParity       :: Maybe PA.Parity
                       , optSwFc         :: Maybe Bool
                       , optHwFc         :: Maybe Bool
                       , optForce        :: Bool
                       , optClearTtyOpts :: Bool
                       , optReload       :: Bool
                       , optConfig       :: String
                       , optPid          :: ProcessID
                       , optPidFile      :: String
                       }
                       deriving ( Show )

-- The defaults for the parameters
defaultOptions :: Options
defaultOptions = Options { optPort         = ""
                         , optTty          = Nothing
                         , optTimeout      = Nothing
                         , optMode         = Nothing
                         , optBaud         = Nothing
                         , optDatabits     = Nothing
                         , optStopBits     = Nothing
                         , optParity       = Nothing
                         , optSwFc         = Nothing
                         , optHwFc         = Nothing
                         , optForce        = False
                         , optClearTtyOpts = False
                         , optReload       = True
                         , optConfig       = "/etc/ser2net.conf"
                         , optPid          = 0
                         , optPidFile      = "/var/run/ser2net.pid"
                         }

data ArgumentException = ArgException String deriving ( Show, Typeable )
instance Exception ArgumentException

readPortArg :: String -> String
readPortArg arg =
  case readMay arg :: Maybe Integer of
    Just n -> if ( n > 1024 ) && ( n < 65536 )
               then
                 arg
               else
                 throw $ ArgException ( "Port " ++ arg ++ " not in a valid range" )

    Nothing -> throw $ ArgException ( "Port arg \"" ++ arg ++ "\" is totally invalid" )

readTtyArg :: String -> Options -> IO Options
readTtyArg arg opts =
  do
    fs <- getFileStatus arg
    unless ( isCharacterDevice fs ) ( throwIO $ ArgException ( "Serial device \"" ++ arg ++ "\" not a character device" ) )
    return opts { optTty = Just arg }


readTimeoutArg :: String -> Maybe Int
readTimeoutArg = readMay

readModeArg :: String -> Maybe PM.PortMode
readModeArg = PM.mapStringToPortMode

readBaudArg :: String -> Maybe BR.BaudRate
readBaudArg = BR.mapStringToBaudRate

readParityArg :: String -> Maybe PA.Parity
readParityArg = PA.mapStringToParity

readConfigArg :: String -> Options -> IO Options
readConfigArg arg opts =
  do
    file_exists <- fileExist arg
    unless file_exists $ throwIO ( ArgException ( "Config file \"" ++ arg ++ "\" doesn't exist" ) )
    fs <- getFileStatus arg
    unless ( isRegularFile fs ) $ throwIO $ ArgException ( "Config file \"" ++ arg ++ "\" is not a regular file" )
    file_accessible <- fileAccess arg True True False
    unless file_accessible $ throwIO $ ArgException ( "Permission problem with file \"" ++ arg ++ "\"" )
    return opts { optConfig = arg }

readPidFileArg :: String -> Options -> IO Options
readPidFileArg arg opts =
  do
    file_exists <- fileExist arg
    unless file_exists $ throwIO ( ArgException ( "Daemon PID file \"" ++ arg ++ "\" doesn't exist" ) )
    fs <- getFileStatus arg
    unless ( isRegularFile fs ) $ throwIO $ ArgException ( "Daemon PID file \"" ++ arg ++ "\" is not a regular file" )
    file_accessible <- fileAccess arg True False False
    when file_accessible $ throwIO $ ArgException ( "Permission problem with file \"" ++ arg ++ "\"" )
    return opts { optPidFile = arg }

readPidArg :: String -> ProcessID
readPidArg arg =
  fromMaybe
    ( throw $ ArgException ( "PID \"" ++ arg ++ "\" is invalid" ) )
    ( readMay arg )



{-
 - Describes the valid options for the app. An array of
 - OptDescr a, where 'a' is (Option -> IO Option).
 -
 - Each element of the array has four components that describe a single option:
 -
 - The arguments to Option (the OptDescr constructor) are:
 -
 -  * list of short option characters
 -  * list of long option strings (without "--")
 -  * argument descriptor
 -  * explanation of option for user
 -
 - Option [Char] [String] (ArgDescr a) String
 -
 - The ArgDescr (argument descriptor) is interesting in this case.
 - Note that the ReqArg and OptArg constructors take functions as
 - arguments: (String -> a) and (Maybe String -> a).
 -
 - Here is the ArgDescr date type:
 -
      data ArgDescr a
        = NoArg                   a         -- no argument expected
        | ReqArg (String       -> a) String -- option requires argument
        | OptArg (Maybe String -> a) String -- optional argument
 -
 - For example, look at the ReqArg constructor of ArgDescr:
 -
        ReqArg (String -> a) String
 -
 - Recall that in this example 'a' is of type (Options -> IO Options).
 -
 - Substituting (Options -> IO Options) for 'a' in (String -> a)
 - we get ...
 -
    String -> (Options -> IO Options ) which is just
    String -> Options -> IO Option
 -
 - Now the types of the lambdas in the List of OptDescr below
 - should make sense.
 -}
optionDescriptions :: [ OptDescr ( Options ->IO Options ) ]
optionDescriptions =
  [ Option "p" [ "port" ]
      ( ReqArg ( \arg opt -> return $ opt { optPort = readPortArg arg } ) "<port>" )
      "The TCP/IP port number of the port of interest"

  , Option "d" [ "tty" ]
      ( ReqArg readTtyArg "<tty device>" )
      "The character device file of the serial device"

  , Option "t" [ "timeout" ]
      ( ReqArg ( \arg opt -> return opt { optTimeout = readTimeoutArg arg } ) "<timeout seconds>" )
      "The number of seconds of inactivity before ser2net will close a tty"

  , Option "m" [ "mode" ]
  ( ReqArg ( \arg opt -> return opt { optMode = readModeArg arg } ) "<off|raw|rawlp|telnet>" )
      "The mode or protocol used to talk to the serial device"

  , Option "b" [ "baud" ]
  ( ReqArg ( \arg opt -> return opt { optBaud = readBaudArg arg } ) "<baud rate>" )
      "The baud rate that the serial device should talk at"

  , Option "w" [ "databits" ]
  ( ReqArg ( \arg opt -> return opt { optDatabits = Just ( if arg == "7" then DB.SevenDataBits else DB.EightDataBits ) } ) "<7|8>" )
      "The number of bits per character for the serial device"

  , Option "s" [ "stopbits" ]
  ( ReqArg ( \arg opt -> return opt { optStopBits = Just ( if arg == "1" then SB.OneStopBit else SB.TwoStopBits ) } ) "<1|2>" )
      "The number of stop bits per character for the serial device"

  , Option "P" [ "parity" ]
  ( ReqArg ( \arg opt -> return opt { optParity = readParityArg arg } ) "<odd|even|none>" )
      "The parity setting for the serial device"

  , Option "x" [ "swfc" ]
  ( NoArg ( \opt -> return opt { optSwFc = Just True } ) )
      "Use software flow control on serial device [default: no flow control]"

  , Option "H" [ "hwfc" ]
  ( NoArg ( \opt -> return opt { optHwFc = Just True } ) )
      "Use hardware flow control on serial device [default: no flow control]"

  , Option "r" [ "force-daemon-restart" ]
  ( NoArg ( \opt -> return opt { optForce = True } ) )
      "If present, causes ser2netctl to be restarted"

  , Option "C" [ "clear-absent-tty-config" ]
  ( NoArg ( \opt -> return opt { optClearTtyOpts = True } ) )
      "Clear any tty options not specified on command line [Default: absent tty options preserved]"

  , Option "n" [ "no-config-reload" ]
  ( NoArg ( \opt -> return opt { optReload = False } ) )
      "If present, prevents the modified ser2net.conf from being reloaded by the daemon"

  , Option "i" [ "pid" ]
  ( ReqArg ( \arg opt -> return opt { optPid = readPidArg arg } ) "<pid>" )
      "Use this to specify the PID of the ser2net daemon"

  , Option "f" [ "pid-file" ]
  ( ReqArg readPidFileArg "<PID file name>")
      "Use this to specify the name of the ser2net daemon's PID file [Default: /var/run/ser2net.pid]"

  , Option "c" [ "config-file" ]
  ( ReqArg readConfigArg "<conf file name>")
      "Use this to specify an alternative config file"

  , Option "v" [ "version" ]
      ( NoArg ( \_ -> putStrLn ( "This is ser2netctl v" ++ version ) >>  exitSuccess ) )
      "Print version information"

  , Option "h" [ "help" ]
  ( NoArg ( \_ -> putStrLn usage_info >>  exitSuccess ) )
      "Print this usage information"
  ]

usage_info :: String
usage_info =  usageInfo "ser2netctl add|remove|update|show|restart|shutdown [options]" optionDescriptions

mergeOptions :: Options -> Options -> Options
mergeOptions opts_from_file cmd_line_opts =
  opts_from_file { optTty       = optTty      ( if isJust $ optTty      cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optTimeout   = optTimeout  ( if isJust $ optTimeout  cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optMode      = optMode     ( if isJust $ optMode     cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optBaud      = optBaud     ( if isJust $ optBaud     cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optDatabits  = optDatabits ( if isJust $ optDatabits cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optStopBits  = optStopBits ( if isJust $ optStopBits cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optParity    = optParity   ( if isJust $ optParity   cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optSwFc      = optSwFc     ( if isJust $ optSwFc     cmd_line_opts then cmd_line_opts else opts_from_file )
                 , optHwFc      = optHwFc     ( if isJust $ optHwFc     cmd_line_opts then cmd_line_opts else opts_from_file )
                 }
 
