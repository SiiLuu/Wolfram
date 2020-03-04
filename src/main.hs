--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- main
--

module Main where

import System.Environment
import Args
import Def
import Tree

main :: IO ()
main = do
    args <- getArgs
    checkPairs (length args)
    let count  = 0
    checkFlag args count
    let rulePos = getPos "--rule"   args
    let ruleVal = read (args !! (rulePos + 1)) :: Int
    let linesPos  = getPos "--lines"  args
    let linesVal  = getValue args linesPos
    let windowPos = getPos "--window" args
    let windowVal = getValue args windowPos 
    let startPos  = getPos "--start"  args
    let startVal  = getValue args startPos
    let movePos   = getPos "--move"   args
    let moveVal   = getValue args movePos
    checkExit rulePos ruleVal
    displayTree ruleVal linesVal windowVal startVal moveVal
