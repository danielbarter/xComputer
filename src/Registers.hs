module Registers where

import Data.Word.Odd
import Data.Word
import Data.Map
import Control.Monad.State.Lazy
import Data.Bits


-- xComputer has 8 registers
type X     = Word16
type Y     = Word16
type FLAG  = Word1
type AC    = Word16
type COUNT = Word4
type PC    = Word10
type IR    = Word16
type ADDR  = Word10



splitIR :: Word16 -> (Word6,Word10)
splitIR ir = (sum $ convert <$> (zip [5,4..0] bits6), sum $ convert <$> (zip [9,8..0] bits10) )
  where (bits6,bits10) = splitAt 6 $ testBit ir <$> [15,14..0]
        convert (i,b) = if b then 2^i else 0



-- main memory
type MEM = Map Word10 Word16


type XComputerState = (X,Y,FLAG,AC,COUNT,PC,IR,ADDR,MEM)

type Step m = StateT XComputerState m


loadXfromAC :: (Monad m) => Step m ()
loadXfromAC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (ac,y,flag,ac,count,pc,ir,addr,mem)

loadYfromMEM :: (Monad m) => Step m ()
loadYfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x, mem ! addr ,flag,ac,count,pc,ir,addr,mem)

loadYfromIR :: (Monad m) => Step m ()
loadYfromIR = do 
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,ir,flag,ac,count,pc,ir,addr,mem)

loadACfromMEM :: (Monad m) => Step m ()
loadACfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,mem ! addr,count,pc,ir,addr,mem)

loadACfromIR :: (Monad m) => Step m ()
loadACfromIR = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ir,count,pc,ir,addr,mem)

incrementAC :: (Monad m) => Step m ()
incrementAC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag, ac+1 ,count,pc,ir,addr,mem)

decrementAC :: (Monad m) => Step m ()
decrementAC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag, ac - 1 ,count,pc,ir,addr,mem)

setCountToZero :: (Monad m) => Step m ()
setCountToZero = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,0,pc,ir,addr,mem)

incrementCount :: (Monad m) => Step m ()
incrementCount = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac, count + 1 ,pc,ir,addr,mem)

loadPCfromIR :: (Monad m) => Step m ()
loadPCfromIR = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,pc') = splitIR ir
  put (x,y,flag,ac,count, pc' ,ir,addr,mem)

loadPCfromMEM :: (Monad m) => Step m ()
loadPCfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,pc') = splitIR $ mem ! addr
  put (x,y,flag,ac,count, pc' ,ir,addr,mem)

incrementPC :: (Monad m) => Step m ()
incrementPC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,count, pc + 1,ir,addr,mem)

loadIRfromMEM :: (Monad m) => Step m ()
loadIRfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,count,pc, mem ! addr ,addr,mem)

loadADDRfromPC :: (Monad m) => Step m ()
loadADDRfromPC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,count,pc,ir,pc,mem)

loadADDRfromIR :: (Monad m) => Step m ()
loadADDRfromIR = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,addr') = splitIR ir
  put (x,y,flag,ac,count,pc,ir, addr' ,mem)

loadADDRfromY :: (Monad m) => Step m ()
loadADDRfromY = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,addr') = splitIR y
  put (x,y,flag,ac,count,pc,ir, addr' ,mem)


---------------------------------------------------------
---------------------------------------------------------


pX     :: XComputerState -> X
pY     :: XComputerState -> Y
pFLAG  :: XComputerState -> FLAG
pAC    :: XComputerState -> AC
pCOUNT :: XComputerState -> COUNT
pPC    :: XComputerState -> PC
pIR    :: XComputerState -> IR
pADDR  :: XComputerState -> ADDR
pMEM   :: XComputerState -> MEM


pX     (x,y,flag,ac,count,pc,ir,addr,mem) = x
pY     (x,y,flag,ac,count,pc,ir,addr,mem) = y
pFLAG  (x,y,flag,ac,count,pc,ir,addr,mem) = flag
pAC    (x,y,flag,ac,count,pc,ir,addr,mem) = ac
pCOUNT (x,y,flag,ac,count,pc,ir,addr,mem) = count
pPC    (x,y,flag,ac,count,pc,ir,addr,mem) = pc
pIR    (x,y,flag,ac,count,pc,ir,addr,mem) = ir
pADDR  (x,y,flag,ac,count,pc,ir,addr,mem) = addr
pMEM   (x,y,flag,ac,count,pc,ir,addr,mem) = mem
