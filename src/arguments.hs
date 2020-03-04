--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- arguments
--

module Args where

import System.Exit
import Data.Maybe
import Text.Read

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