module PortMode where

import Data.Char

data PortMode = Off | Raw | RawLp | Telnet deriving ( Eq )

instance Show PortMode where
  show Off = "off"
  show Raw = "raw"
  show RawLp = "rawlp"
  show Telnet = "telnet"

mapStringToPortMode :: String -> Maybe PortMode
mapStringToPortMode s =
  let
    lower_s = map toLower s
    pm_map = [ ( "off", Off )
             , ( "raw", Raw)
             , ( "rawlp", RawLp)
             , ( "telnet", Telnet)
             ]
  in
    lookup lower_s pm_map




