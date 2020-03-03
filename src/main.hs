--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- main
--

module Main where

import System.Environment
import System.Exit
import Data.Maybe
import Data.List

checkExit :: Int -> IO () 
checkExit rule
    | rule == -1 = errorExit
    | otherwise = return ()

errorExit :: IO ()
errorExit = do
    putStrLn "Bad inputs flags"
    exitWith (ExitFailure 84)

defaultVal :: String -> Int
defaultVal elem
    | elem == "--rule"   = -1
    | elem == "--lines"  = 0
    | elem == "--window" = 80
    | elem == "--start"  = 0
    | elem == "--move"   = 0

getPos :: String -> [String] -> Int
getPos elem list = fromMaybe (defaultVal elem) (elemIndex elem list)

main :: IO ()
main = do
    args <- getArgs

    let rule   = getPos "--rule"   args
    let lines  = getPos "--lines"  args
    let window = getPos "--window" args
    let start  = getPos "--start"  args
    let move   = getPos "--move"   args
    checkExit rule

    print rule
    print lines
    print window
    print start
    print move
