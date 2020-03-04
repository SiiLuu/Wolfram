--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- default
--

module Def where

import Data.Maybe
import Text.Read
import Data.List

defaultVal :: String -> Int
defaultVal elem
    | elem == "--rule"   = -1
    | elem == "--lines"  = 0
    | elem == "--window" = 80
    | elem == "--start"  = 0
    | elem == "--move"   = 0

getPos :: String -> [String] -> Int
getPos elem list = fromMaybe (defaultVal elem) (elemIndex elem list)

getValue :: [String] -> Int -> Int
getValue list val
    | val == 0 = 0
    | val == 80 = 80
    | otherwise = read (list !! (val + 1)) :: Int