--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- tree 
--

module Tree where

import Data.Maybe
import Text.Read
import Data.List
import Rule30
import Rule90
import Rule110

getBlinksEnd :: Int -> Int -> String -> String
getBlinksEnd window space tree
    | space <= 0 && window `mod` 2 /= 0 = getBlinksEnd (window - 1) (space - 1) (" " ++ tree)
    | space <= 0 = tree
    | otherwise = getBlinksEnd window (space - 1) (" " ++ tree)

getBlinks :: Int -> String -> String
getBlinks space tree
    | space <= 0 = tree
    | otherwise = getBlinks (space - 1) (" " ++ tree)

generation :: String -> String -> Int -> Int -> IO ()
generation str finalStr rule lines
    | rule == 30 = rule30 str finalStr rule lines
    | rule == 90 = rule90 str finalStr rule lines
    | otherwise  = rule110 str finalStr rule lines

loop :: Int -> Int -> Int -> String -> Int -> IO ()
loop rule lines space tree window = do
    let str = (getBlinks space "" ++ tree ++ getBlinksEnd window (space - 1) "")
    putStrLn str
    let finalStr = ""
    generation str finalStr rule lines 

displayTree :: Int -> Int -> Int -> Int -> Int -> IO ()
displayTree rule lines window start move = do
    let tree = "*"
    let esp = (window `div` 2)
    loop rule lines esp tree window
