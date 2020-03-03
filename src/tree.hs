--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- tree
--

module Tree where

getBlinks :: Int -> Int -> String -> String
getBlinks window count tree
    | count == window = tree
    | otherwise = getBlinks window (count + 1) (" " ++ tree)

loop :: Int -> Int -> String -> IO ()
loop lines count tree
    | lines == 0 = do
        putStrLn tree
        loop lines count (tree ++ "*")
    | count == lines = return ()
    | otherwise = do
        putStrLn tree
        loop lines (count + 1) (tree ++ "*")

displayTree :: Int -> Int -> Int -> Int -> Int -> IO ()
displayTree rule lines window start move = do
    let count = 0
    let str = "*"
    let tree = getBlinks window count str
    loop lines count tree