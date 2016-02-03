{-# LANGUAGE BinaryLiterals #-}

module Main where

import Machine
import AssemblerParser
import AssemblerTranslator
import Text.Parsec (parse)

parseTest :: IO ()
parseTest = do s <- readFile "programs/multiply.xasm"
               let o = parse assemblerParser "" s
               case o of Left e  -> putStrLn $ show e
                         Right p -> putStrLn $ show $ produceMemoryMap p 0

main = parseTest
