--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- tree
--

module Tree where

getBlinks :: Int -> String -> String
getBlinks window tree
    | window <= 0 = tree
    | otherwise = getBlinks (window - 1) (" " ++ tree)

loop :: Int -> Int -> IO ()
loop lines space
    | lines == 0 = do
        let str = getBlinks space " "
        putStrLn str
        loop lines (space - 1)

    | lines <= 1 = return ()
    | otherwise = do
        let str = getBlinks space " "
        putStrLn str
        loop (lines - 1) (space - 1)

displayTree :: Int -> Int -> Int -> Int -> Int -> IO ()
displayTree rule lines window start move = do
    loop lines window
