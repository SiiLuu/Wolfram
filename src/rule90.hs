--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- rule90
--

module Rule90 where

withoutStart ::  String -> String -> Int -> Int-> Int-> Int-> Int-> IO ()
withoutStart str finalStr lines start window spaces power = do
    let size = 0
    let final = changeStr90 str finalStr size
    let begin = drop (power - spaces) final
    let end = take window begin
    putStrLn end
    rule90 final finalStr (lines - 1) start window spaces power

withStart ::  String -> String -> Int -> Int-> Int-> Int-> Int-> IO ()
withStart str finalStr lines start window spaces power = do
    let size = 0
    let final = changeStr90 str finalStr size
    rule90 final finalStr lines (start - 1) window spaces power

rule90 :: String -> String -> Int -> Int -> Int -> Int -> Int -> IO ()
rule90 str finalStr lines start window spaces power
    | lines == 0 || lines <= 1 = return ()
    | start > 0 = withStart str finalStr lines start window spaces power
    | otherwise = withoutStart str finalStr lines start window spaces power

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