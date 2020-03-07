--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- rule90
--

module Rule90 where

infinite :: String -> String -> Int -> Int-> Int -> Int -> Int -> IO ()
infinite str finalStr lines start window spaces power
    | start > 0 = do
        let size = 0
        let final = changeStr90 str finalStr size
        infinite final finalStr lines (start - 1) window spaces power
    | otherwise = do
        let size = 0
        let final = changeStr90 str finalStr size
        let a = drop (power - spaces) final
        let b = take window a
        putStrLn b
        infinite final finalStr lines start window spaces power

rule90 :: String -> String -> Int -> Int -> Int -> Int -> Int -> IO ()
rule90 str finalStr lines start window spaces power
    | lines == 0 = infinite str finalStr lines start window spaces power
    | lines <= 1 = return ()
    | start > 0 = do
        let size = 0
        let final = changeStr90 str finalStr size
        rule90 final finalStr lines (start - 1) window spaces power
    | otherwise = do
        let size = 0
        let final = changeStr90 str finalStr size
        let a = drop (power - spaces) final
        let b = take window a
        putStrLn b
        rule90 final finalStr (lines - 1) start window spaces power

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