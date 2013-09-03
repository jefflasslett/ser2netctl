module Parity where

import Data.Char

data Parity = None | Odd | Even deriving ( Eq )

instance Show Parity where
  show None = "NONE"
  show Odd = "ODD"
  show Even = "EVEN"

mapStringToParity :: String -> Maybe Parity
mapStringToParity s =
  let
    upper_s = map toUpper s
    parity_map = [ ( "NONE", None )
                 , ( "ODD", Odd )
                 , ( "EVEN", Even )
                 ]
  in
    lookup upper_s parity_map




