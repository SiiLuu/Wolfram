--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- rule90
--

module Rule90 where

infinite :: String -> String -> Int -> Int-> IO ()
infinite str finalStr lines start
    | start > 0 = do
        let size = 0
        let final = changeStr90 str finalStr size
        infinite final finalStr lines (start - 1)
    | otherwise = do
        let size = 0
        let final = changeStr90 str finalStr size
        putStrLn final
        infinite final finalStr lines start

rule90 :: String -> String -> Int -> Int -> IO ()
rule90 str finalStr lines start
    | lines == 0 = infinite str finalStr lines start
    | lines <= 1 = return ()
    | start > 0 = do
        let size = 0
        let final = changeStr90 str finalStr size
        rule90 final finalStr lines (start - 1)
    | otherwise = do
        let size = 0
        let final = changeStr90 str finalStr size
        putStrLn final
        rule90 final finalStr (lines - 1) start

toolarge :: String -> String -> Int -> String
toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == '*' = changeStr90 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' = changeStr90 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr90 str (finalStr ++ " ") (size + 1)

negative :: String -> String -> Int -> String
negative str finalStr size
    | (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr90 str (finalStr ++ "*") (size + 1)
    | (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr90 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr90 str (finalStr ++ " ") (size + 1)

changeStr90 :: String -> String -> Int -> String
changeStr90 str finalStr size
    | size >= length (str) = finalStr
    | size == 0 = negative str finalStr size
    | size == (length (str) - 1) = toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr90 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' && (str !! (size + 1)) == ' ' = changeStr90 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr90 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr90 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr90 str (finalStr ++ " ") (size + 1)