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

getBlinks :: Int -> String -> String
getBlinks window tree
    | window <= 0 = tree
    | otherwise = getBlinks (window - 1) (" " ++ tree)

windowed :: Int -> Int -> String -> Int -> IO ()
windowed lines space tree window
    | length (tree) >= window = loop (lines - 1) (space - 1) (tree) window
    | (length (tree) + 1) >= window = loop (lines - 1) (space - 1) (tree ++ "*") window
    | otherwise = loop (lines - 1) (space - 1) (tree ++ "**") window

loop :: Int -> Int -> String -> Int -> IO ()
loop lines space tree window
    | lines <= 1 = return ()
    | otherwise = do
        let str = (getBlinks space "" ++ tree ++ getBlinks (space - 1) "")
        putStrLn str
        windowed lines space tree window

displayTree :: Int -> Int -> Int -> Int -> Int -> IO ()
displayTree rule lines window start move = do
    let tree = "*"
    let esp = (window `div` 2)
    loop lines esp tree window
