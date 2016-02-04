{-# LANGUAGE BinaryLiterals #-}

module Main where

import Prelude hiding (cycle)
import Machine
import AssemblerParser
import AssemblerTranslator
import Text.Parsec (parse)
import System.IO
import PpComputerState
import Control.Monad.State.Lazy

fetchASM :: IO [ASM]
fetchASM = do putStr "Path to .xasm file: "
              f <- getLine
              s <- readFile f 
              let x = parse assemblerParser f s
              case x of Left e  -> (putStrLn $ show e) >> fetchASM
                        Right p -> return p  
                
produceInitialState :: [ASM] -> IO XComputerState
produceInitialState p = return (0,0,0,0,0,0,0,0,m)
  where m = produceMemoryMap p 0


main = do hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          p <- fetchASM
          s <- produceInitialState p
          putStrLn $ ppComputerState s
          go s

go :: XComputerState -> IO ()
go s = do (instruction,s') <- runStateT (cycle) s
          putStrLn $ ppComputerState s'
          case instruction of HLT -> return ()
                              _   -> go s'
