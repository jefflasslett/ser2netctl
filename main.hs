module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import qualified Control.Exception as CE
import Control.Monad

import Data.Maybe

import qualified Options as O
import qualified Commands as C


printIndentedListWithHeading :: String -> [ String ] -> IO ()
printIndentedListWithHeading _ [] = return ()
printIndentedListWithHeading h xs =
  do
    putStrLn ""
    putStrLn h
    -- Map over the strings, printing them out.
    mapM_ putStrLn $ zipWith (++) (map ( (\s -> "  " ++ s ++ " ") . show ) [1 :: Int ..] ) xs
    putStrLn ""

errorOut :: String -> Int -> Bool -> IO ()
errorOut msg rc print_usage =
  hPutStrLn stderr outStr
  >>
  when print_usage ( putStrLn O.usage_info )
  >>
  exitWith ( ExitFailure rc )
    where
      outStr = "ser2netctl: " ++ msg

handleArgException :: O.ArgumentException -> IO O.Options
handleArgException ( O.ArgException s ) =
  errorOut s ( -1 ) False >> return O.defaultOptions


main :: IO ()
main =
  do
    args <- getArgs

    let
      cmndArg = head args
      optArgs = tail args

    when ( null args ) $ errorOut "No arguments.  I need a command arg at least!" ( -1 ) True

    unless ( C.validCommand cmndArg ) $ errorOut ( "Command \"" ++ cmndArg ++ "\" not recognised.  Game over!") ( -1 ) True

    putStrLn $ "Got command arg: " ++ cmndArg

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute O.optionDescriptions optArgs

    -- Print out the errors.
    unless ( null errors ) $ printIndentedListWithHeading "Errors" errors
    unless ( null errors ) $ errorOut "Exiting due to bad args on the command line!" ( -1 ) True

    -- Here we thread startOptions through all supplied option actions
    opts <- CE.catch ( foldl (>>=) (return O.defaultOptions) actions ) handleArgException

    putStrLn ( show opts )

    -- fromJust will work as cmndArg already known valid
    C.execCommand ( fromJust $ C.mapStringToCmnd cmndArg ) opts

    putStrLn "Good-bye!"






