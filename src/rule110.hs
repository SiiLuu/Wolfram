--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- rule110
--

module Rule110 where

withoutStart ::  String -> String -> Int -> Int-> Int-> Int-> Int-> IO ()
withoutStart str finalStr lines start window spaces power = do
    let size = 0
    let final = changeStr110 str finalStr size
    let begin = drop (power - spaces) final
    let end = take window begin
    putStrLn end
    rule110 final finalStr (lines - 1) start window spaces power

withStart ::  String -> String -> Int -> Int-> Int-> Int-> Int-> IO ()
withStart str finalStr lines start window spaces power = do
    let size = 0
    let final = changeStr110 str finalStr size
    rule110 final finalStr lines (start - 1) window spaces power

rule110 :: String -> String -> Int -> Int -> Int -> Int -> Int -> IO ()
rule110 str finalStr lines start window spaces power
    | lines == 0 || lines <= 1 = return ()
    | start > 0 = withStart str finalStr lines start window spaces power
    | otherwise = withoutStart str finalStr lines start window spaces power

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