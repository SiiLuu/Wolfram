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

getBlinksEnd :: Int -> Int -> String -> String
getBlinksEnd window space tree
    | space <= 0 && window `mod` 2 /= 0 = getBlinksEnd (window - 1) (space - 1) (" " ++ tree)
    | space <= 0 = tree
    | otherwise = getBlinksEnd window (space - 1) (" " ++ tree)

getBlinks :: Int -> String -> String
getBlinks space tree
    | space <= 0 = tree
    | otherwise = getBlinks (space - 1) (" " ++ tree)

toolarge :: String -> String -> Int -> String
toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' = changeStr str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' = changeStr str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr str (finalStr ++ " ") (size + 1)

negative :: String -> String -> Int -> String
negative str finalStr size
    | (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr str (finalStr ++ "*") (size + 1)
    | (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr str (finalStr ++ "*") (size + 1)
    | (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr str (finalStr ++ " ") (size + 1)

changeStr :: String -> String -> Int -> String
changeStr str finalStr size
    | size >= length (str) = finalStr
    | size == 0 = negative str finalStr size
    | size == (length (str) - 1) = toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' && (str !! (size + 1)) == ' ' = changeStr str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr str (finalStr ++ " ") (size + 1)

generation :: String -> String -> Int -> IO ()
generation str finalStr lines
    | lines <= 1 = return ()
    | otherwise = do
        let size = 0
        let final = changeStr str finalStr size
        putStrLn final
        generation final finalStr (lines - 1)

loop :: Int -> Int -> String -> Int -> IO ()
loop lines space tree window = do
    let str = (getBlinks space "" ++ tree ++ getBlinksEnd window (space - 1) "")
    putStrLn str
    let finalStr = ""
    generation str finalStr lines 

displayTree :: Int -> Int -> Int -> Int -> Int -> IO ()
displayTree rule lines window start move = do
    let tree = "*"
    let esp = (window `div` 2)
    loop (lines + 1) esp tree window
