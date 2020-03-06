--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- rule30
--

module Rule30 where

rule30 :: String -> String -> Int -> Int -> Int-> IO ()
rule30 str finalStr rule lines start
    | lines <= 1 = return ()
    | start > 0 = do
        let size = 0
        let final = changeStr30 str finalStr size
        rule30 final finalStr rule lines (start - 1)
    | otherwise = do
        let size = 0
        let final = changeStr30 str finalStr size
        putStrLn final
        rule30 final finalStr rule (lines - 1) start

toolarge :: String -> String -> Int -> String
toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' = changeStr30 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' = changeStr30 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr30 str (finalStr ++ " ") (size + 1)

negative :: String -> String -> Int -> String
negative str finalStr size
    | (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr30 str (finalStr ++ "*") (size + 1)
    | (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr30 str (finalStr ++ "*") (size + 1)
    | (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr30 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr30 str (finalStr ++ " ") (size + 1)

changeStr30 :: String -> String -> Int -> String
changeStr30 str finalStr size
    | size >= length (str) = finalStr
    | size == 0 = negative str finalStr size
    | size == (length (str) - 1) = toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' && (str !! (size + 1)) == ' ' = changeStr30 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr30 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr30 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr30 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr30 str (finalStr ++ " ") (size + 1)