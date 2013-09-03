module StopBits where

import Data.Char

data StopBits = OneStopBit | TwoStopBits deriving ( Eq )

instance Show StopBits where
  show OneStopBit = "1STOPBIT"
  show TwoStopBits = "2STOPBITS"

mapStringToStopBits :: String -> Maybe StopBits
mapStringToStopBits s =
  let
    upper_s = map toUpper s
    sb_map = [ ( "1STOPBIT", OneStopBit )
             , ( "2STOPBITS", TwoStopBits)
             ]
  in
    lookup upper_s sb_map




