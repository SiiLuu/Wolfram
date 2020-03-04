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
import Text.Read
import Tree

checkPairs :: Int -> IO ()
checkPairs val
    | val `mod` 2 /= 0 = errorExit
    | otherwise = return ()

checkExit :: Int -> Int -> IO ()
checkExit pos val
    | pos == -1 = errorExit
    | val == 30 = return ()
    | val == 90 = return ()
    | val == 110 = return ()
    | otherwise = errorExit

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

checkFlag :: [String] -> Int -> IO ()
checkFlag list count
    | count >= (length list) = return ()
    | list !! count == "--rule"   = checkValue list (count + 1)
    | list !! count == "--lines"  = checkValue list (count + 1)
    | list !! count == "--window" = checkValue list (count + 1)
    | list !! count == "--start"  = checkValue list (count + 1)
    | list !! count == "--move"   = checkValue list (count + 1)
    | otherwise = errorExit

checkValue :: [String] -> Int -> IO ()
checkValue list count
    | count == (length list) = return ()
    | (readMaybe (list !! count) :: Maybe Int) /= Nothing = checkFlag list (count + 1)
    | otherwise = errorExit

getValue :: [String] -> Int -> Int
getValue list val
    | val == 0 = 0
    | val == 80 = 80
    | otherwise = read (list !! (val + 1)) :: Int

main :: IO ()
main = do
    args <- getArgs
    checkPairs (length args)
    let count  = 0
    checkFlag args count
    let rulePos = getPos "--rule"   args
    let ruleVal = read (args !! (rulePos + 1)) :: Int
    let linesPos  = getPos "--lines"  args
    let linesVal  = getValue args linesPos
    let windowPos = getPos "--window" args
    let windowVal = getValue args windowPos 
    let startPos  = getPos "--start"  args
    let startVal  = getValue args startPos
    let movePos   = getPos "--move"   args
    let moveVal   = getValue args movePos
    checkExit rulePos ruleVal
    displayTree ruleVal linesVal windowVal startVal moveVal
