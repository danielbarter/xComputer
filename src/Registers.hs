module Registers where

import Data.Word.Odd
import Data.Word
import Data.Map
import Control.Monad.State.Lazy


-- xComputer has 8 registers
type X     = Word16
type Y     = Word16
type FLAG  = Word1
type AC    = Word16
type COUNT = Word4
type PC    = Word10
type IR    = Word16
type ADDR  = Word10


-- main memory
type MEM = Map Word10 Word16
