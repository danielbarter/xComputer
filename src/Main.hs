{-# LANGUAGE BinaryLiterals #-}

module Main where

import Machine
import Prelude hiding (cycle)
import Control.Monad.State.Lazy
import Data.Map
import Data.Word

main :: IO ()
main = do
  putStrLn "hello world"
