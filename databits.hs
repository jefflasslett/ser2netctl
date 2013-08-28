module DataBits where

import Data.Char

data DataBits = SevenDataBits | EightDataBits deriving ( Eq )

instance Show DataBits where
  show SevenDataBits = "7DATABITS"
  show EightDataBits = "8DATABITS"

mapStringToDataBits :: String -> Maybe DataBits
mapStringToDataBits s =
  let
    upper_s = map toUpper s
    db_map = [ ( "7DATABITS", SevenDataBits )
             , ( "8DATABITS", EightDataBits)
             ]
  in
    lookup upper_s db_map




