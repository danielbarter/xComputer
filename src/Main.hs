module Main where

import Machine
import Prelude hiding (cycle)
import Control.Monad.State.Lazy
import Data.Map
import Data.Word

initialState :: XComputerState
initialState = (0,0,0,0,0,0,0,0,memory)

fromBitPattern :: [Int] -> Word16
fromBitPattern l = sum $ convert  <$> (zip [15,14..0] $ bitToBool <$> l)
  where convert (i,b) = if b then 2^i else 0
        bitToBool 0 = False
        bitToBool 1 = True

memory :: MEM
memory = fromList $ zip [0,1..(2^10-1)]
 (
 (fromBitPattern [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]) :
 [0,0..]
 )





main :: IO ()
main = do
  putStrLn "hello world"
