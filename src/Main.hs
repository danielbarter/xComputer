{-# LANGUAGE BinaryLiterals #-}

module Main where

import Machine
import Assembler
import Text.Parsec (parse)

parseTest :: IO ()
parseTest = do s <- readFile "programs/multiply.xasm"
               let p = parse assemblerParser "" s
               putStrLn $ show p

main = parseTest
