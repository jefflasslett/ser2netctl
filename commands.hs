module Commands ( validCommand, Cmnd( .. ), mapStringToCmnd, execCommand ) where

import Data.Char
import Data.Maybe

import Options as O

data Cmnd = Add | Remove | Start | Stop | Update | Show | Restart | Shutdown deriving ( Show, Eq )

mapStringToCmnd :: String -> Maybe Cmnd
mapStringToCmnd s =
  let
    string_cmnd_map = [ ( "add", Add )
                      , ( "remove", Remove )
                      , ( "restart", Restart )
                      , ( "show", Show )
                      , ( "shutdown", Shutdown )
                      , ( "start", Start )
                      , ( "stop", Stop )
                      , ( "update", Update )
                      ]
    lower_s = map toLower s
  in
    lookup lower_s string_cmnd_map

validCommand :: String -> Bool
validCommand str =
  isJust $ mapStringToCmnd str

execCommand :: Cmnd -> O.Options -> IO ()
execCommand c opts = 
  undefined

execAdd :: O.Options -> IO ()
execAdd opts =
  undefined

execRemove :: O.Options -> IO ()
execRemove opts =
  undefined

execRestart :: O.Options -> IO ()
execRestart opts =
  undefined

execStart :: O.Options -> IO ()
execStart opts =
  undefined

execStop :: O.Options -> IO ()
execStop opts =
  undefined
execShow :: O.Options -> IO ()
execShow opts =
  undefined

execShutdown :: O.Options -> IO ()
execShutdown opts =
  undefined

execUpdate :: O.Options -> IO ()
execUpdate opts =
  undefined




