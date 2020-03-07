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

generation :: String -> String -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
generation str finalStr rule lines start window spaces power
    | rule == 30 = rule30 str finalStr lines start window spaces power
    | rule == 90 = rule90 str finalStr lines start window spaces power
    | otherwise  = rule110 str finalStr lines start window spaces power

loop :: Int -> Int -> Int -> String -> Int -> Int -> Int -> IO ()
loop rule lines space tree window start move
    | start /= 0 = do
        let str = (getBlinks ((space + move) * lines) "" ++ tree ++ getBlinksEnd window ((space + move) * lines - 1) "")
        let finalStr = ""
        generation str finalStr rule (lines + 1) (start - 1) window (move + space) (space * lines)
    | otherwise = do
        let str = (getBlinks ((space + move) * lines) "" ++ tree ++ getBlinksEnd window ((space + move) * lines - 1) "")
        let first = drop (((space + move) * lines) - (move + space )) str
        let final = take window first
        putStrLn final
        let finalStr = ""
        generation str finalStr rule lines start window (move + space) ((space + move) * lines)

displayTree :: Int -> Int -> Int -> Int -> Int -> IO ()
displayTree rule lines window start move = do
    let tree = "*"
    let esp = (window `div` 2)
    loop rule lines esp tree window start move
