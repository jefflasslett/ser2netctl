module BaudRate where

data BaudRate = B0
              | B50
              | B75
              | B110
              | B134
              | B150
              | B200
              | B300
              | B600
              | B1200
              | B1800
              | B2400
              | B4800
              | B9600
              | B19200
              | B38400
              | B57600
              | B115200
              | B230400
              | B460800
              | B921600
              deriving ( Eq )

instance Show BaudRate where
  show B0       = "0"
  show B50      = "50"
  show B75      = "75"
  show B110     = "110"
  show B134     = "134"
  show B150     = "150"
  show B200     = "200"
  show B300     = "300"
  show B600     = "600"
  show B1200    = "1200"
  show B1800    = "1800"
  show B2400    = "2400"
  show B4800    = "4800"
  show B9600    = "9600"
  show B19200   = "19200"
  show B38400   = "38400"
  show B57600   = "57600"
  show B115200  = "115200"
  show B230400  = "230400"
  show B460800  = "460800"
  show B921600  = "921600"
 
mapStringToBaudRate :: String -> Maybe BaudRate
mapStringToBaudRate s =
  let
    br_map = [ ( "0",      B0 )
             , ( "50",     B50 )
             , ( "75",     B75 )
             , ( "110",    B110 )
             , ( "134",    B134 )
             , ( "150",    B150 )
             , ( "200",    B200 )
             , ( "300",    B300 )
             , ( "600",    B600 )
             , ( "1200",   B1200 )
             , ( "1800",   B1800 )
             , ( "2400",   B2400 )
             , ( "4800",   B4800 )
             , ( "9600",   B9600 )
             , ( "19200",  B19200 )
             , ( "38400",  B38400 )
             , ( "57600",  B57600 )
             , ( "115200", B115200 )
             , ( "230400", B230400 )
             , ( "460800", B460800 )
             , ( "921600", B921600 )
             ]
  in
    lookup s br_map




