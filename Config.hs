module Config where

import Data.List
import Data.List.Split
import Data.Maybe

import Text.Regex.PCRE

import Safe

import Options

import qualified DataBits as DB
import qualified BaudRate as BR
import qualified Parity   as PA
import qualified PortMode as PM
import qualified StopBits as SB


makeConfigLine :: Options -> String
makeConfigLine opts =
  let
    ttyParms = makeTtyParamsString opts
    elements = [ optPort opts
               , maybeToString $ optMode opts
               , maybeToString $ optTimeout opts
               , fromMaybe "" ( optTty opts )
               ]

    elements' = if not $ null ttyParms then elements ++ [ ttyParms ] else elements
  in
    intercalate ":" elements'

maybeToString :: ( Show a ) => Maybe a -> String
maybeToString ( Just n ) = show n
maybeToString Nothing = ""

makeTtyParamsString :: Options -> String
makeTtyParamsString opts =
  let
    ttyOptList = [ maybeToString $ optBaud opts
                 , maybeToString $ optParity opts
                 , maybeToString $ optStopBits opts
                 , maybeToString $ optDatabits opts
                 , case optSwFc opts of
                     Nothing -> ""
                     Just True -> "XONXOFF"
                     Just False -> "-XONXOFF"
                 , case optHwFc opts of
                     Nothing -> ""
                     Just True -> "RTSCTS"
                     Just False -> "-RTSCTS"
                 ]

    optsInUse = filter ( not.null ) ttyOptList
  in
    unwords optsInUse

parseConfigLine :: String -> Maybe Options
parseConfigLine s =
  let
    port_pattern = "\\d+:\\w+:\\d+:/dev/\\w+"
    is_port_cfg = s =~ port_pattern :: Bool
    main_parts = splitOn ":" s
    port = head main_parts
    mode = PM.mapStringToPortMode ( main_parts !! 1 )
    timeout = readMay ( main_parts !! 2 )
    dev = Just $ main_parts !! 3

    main_opts = defaultOptions { optPort = port
                               , optMode = mode
                               , optTimeout = timeout
                               , optTty = dev
                               }

    tty_parms_str = main_parts !! 4
    tty_opts = parseTtyParms main_opts tty_parms_str
  in
    if is_port_cfg
      then
        Just $ if length main_parts == 5 then tty_opts else main_opts
      else
        Nothing


parseTtyParms :: Options -> String -> Options
parseTtyParms opts s =
  let
    tty_parm_parts = splitOneOf " ," s
    f :: Options -> String -> Options
    f o p =
      let
        m = [ ( "^\\d+$", \o' s' -> o' { optBaud = BR.mapStringToBaudRate s' } )
            , ( "(NONE)|(ODD)|(EVEN)", \o' s' -> o' { optParity = PA.mapStringToParity s' } )
            , ( "DATABIT", \o' s' -> o' { optDatabits = DB.mapStringToDataBits s' } )
            , ( "STOPBIT", \o' s' -> o' { optStopBits = SB.mapStringToStopBits s' } )
            , ( "^XONXOFF", \o' s' -> o' { optSwFc = Just True } )
            , ( "^-XONXOFF", \o' s' -> o' { optSwFc = Just False } )
            , ( "^RTSCTS", \o' s' -> o' { optHwFc = Just True } )
            , ( "^-RTSCTS", \o' s' -> o' { optHwFc = Just False } )
            ]

        e = find ( \pair -> p =~ fst pair :: Bool ) m
      in
        case e of
          Just pair -> snd pair o p
          Nothing -> o
  in
    foldl' f opts tty_parm_parts


