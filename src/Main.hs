{-# LANGUAGE BinaryLiterals #-}

module Main where

import Machine
import Assembler
import Text.Parsec (parse)

parseTest :: IO ()
parseTest = do s <- readFile "programs/test.xasm"
               let p = parse assemblerParser "" s
               putStrLn $ show p

main = parseTest
