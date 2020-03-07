--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- rule110
--

module Rule110 where

infinite :: String -> String -> Int -> Int -> Int -> Int -> Int -> IO ()
infinite str finalStr lines start window spaces power
    | start > 0 = do
        let size = 0
        let final = changeStr110 str finalStr size
        infinite final finalStr lines (start - 1) window spaces power
    | otherwise = do
        let size = 0
        let final = changeStr110 str finalStr size
        let a = drop (power - spaces) final
        let b = take window a
        putStrLn b
        infinite final finalStr lines start window spaces power

rule110 :: String -> String -> Int -> Int -> Int -> Int -> Int -> IO ()
rule110 str finalStr lines start window spaces power
    | lines == 0 = return ()
    | lines <= 1 = return ()
    | start > 0 = do
        let size = 0
        let final = changeStr110 str finalStr size
        rule110 final finalStr lines (start - 1) window spaces power
    | otherwise = do
        let size = 0
        let final = changeStr110 str finalStr size
        let a = drop (power - spaces) final
        let b = take window a
        putStrLn b
        rule110 final finalStr (lines - 1) start window spaces power

toolarge :: String -> String -> Int -> String
toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == '*' = changeStr110 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' = changeStr110 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr110 str (finalStr ++ " ") (size + 1)

negative :: String -> String -> Int -> String
negative str finalStr size
    | (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr110 str (finalStr ++ "*") (size + 1)
    | (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr110 str (finalStr ++ "*") (size + 1)
    | (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr110 str (finalStr ++ "*") (size + 1)
    | otherwise = changeStr110 str (finalStr ++ " ") (size + 1)

changeStr110 :: String -> String -> Int -> String
changeStr110 str finalStr size
    | size >= length (str) = finalStr
    | size == 0 = negative str finalStr size
    | size == (length (str) - 1) = toolarge str finalStr size
    | (str !! (size - 1)) == '*' && (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr110 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == '*' && (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr110 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' && (str !! (size + 1)) == '*' = changeStr110 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == '*' && (str !! (size + 1)) == ' ' = changeStr110 str (finalStr ++ "*") (size + 1)
    | (str !! (size - 1)) == ' ' && (str !! size) == ' ' && (str !! (size + 1)) == '*' = changeStr110 str (finalStr ++ "*") (size + 1)    
    | otherwise = changeStr110 str (finalStr ++ " ") (size + 1)