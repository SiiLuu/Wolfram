--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- rule90
--

module Rule90 where

rule90 :: String -> String -> Int -> Int -> IO ()
rule90 str finalStr rule lines
    | lines <= 1 = return ()
    | otherwise = do
        let size = 0
        let final = changeStr90 str finalStr size
        putStrLn final
        rule90 final finalStr rule (lines - 1)

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