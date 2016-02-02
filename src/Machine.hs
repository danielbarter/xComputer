module Machine where

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

type InstructionCode = Word6

data Instruction = ADD
                 | SUB
                 | AND
                 | OR
                 | NOT
                 | SHL
                 | SHR
                 | INC
                 | DEC
                 | LOD
                 | STO
                 | HLT
                 | JMP
                 | JMZ
                 | JMN
                 | JMF

translate :: Word6 -> Instruction
translate n = case n of 0  -> ADD
                        1  -> SUB
                        2  -> AND
                        3  -> OR
                        4  -> NOT
                        5  -> SHL
                        6  -> SHR
                        7  -> INC
                        8  -> DEC
                        9  -> LOD
                        10 -> STO
                        11 -> HLT
                        12 -> JMP
                        13 -> JMZ
                        14 -> JMN
                        15 -> JMF
                        _  -> HLT -- unrecognized instructions sent to HLT

type XComputerState = (X,Y,FLAG,AC,COUNT,PC,IR,ADDR,MEM)

type Step m = StateT XComputerState m


------------------------------------------------
------------------------------------------------


fetch :: (Monad m) => Step m Instruction
fetch = do
  loadADDRfromPC
  loadIRfromMEM
  incrementPC
  s <- get
  let (instruction,_) = splitIR $ pIR s
  return $ translate instruction

-- needs refactoring really bad
execute :: (Monad m) => Instruction -> Step m Instruction
execute i = case i of ADD -> do loadADDRfromIR
                                loadXfromAC
                                loadYfromMEM
                                (carry,sum) <- aluADD
                                loadACfromALU sum
                                loadFLAGfromALU carry
                                return i

                      SUB -> do loadADDRfromIR
                                loadXfromAC
                                loadYfromMEM
                                (flag,dif) <- aluSUB
                                loadACfromALU dif
                                loadFLAGfromALU flag
                                return i

                      AND -> do loadADDRfromIR
                                loadXfromAC
                                loadYfromMEM
                                (flag,ac) <- aluAND
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i

                      OR  -> do loadADDRfromIR
                                loadXfromAC
                                loadYfromMEM
                                (flag,ac) <- aluOR
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i

                      NOT -> do loadXfromAC
                                (flag,ac) <- aluNOT
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i

                      SHL -> do loadXfromAC
                                (flag,ac) <- aluSHL
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i

                      SHR -> do loadXfromAC
                                (flag,ac) <- aluSHR
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i

                      INC -> do incrementAC
                                return i

                      DEC -> do decrementAC
                                return i

                      LOD -> do loadADDRfromIR
                                loadACfromMEM
                                return i

                      STO -> do loadADDRfromIR
                                loadMEMfromAC
                                return i
                                                                
                      HLT -> do incrementCount 
                                return i

                      JMP -> do loadPCfromIR
                                return i

                      JMZ -> do b <- acZero
                                if b then loadPCfromIR >> (return i) else (return i)

                      JMN -> do b <- acNegative
                                if b then loadPCfromIR >> (return i) else (return i)

                      JMF -> do b <- flagSet
                                if b then loadPCfromIR >> (return i) else (return i)


cycle :: (Monad m) => Step m Instruction
cycle = fetch >>= execute

-------------------------------------------------
-------------------------------------------------

incrementCount :: (Monad m) => Step m ()
incrementCount = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac, count + 1 ,pc,ir,addr,mem)


setCountToZero :: (Monad m) => Step m ()
setCountToZero = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,0,pc,ir,addr,mem)


-------------------------------------------------
-------------------------------------------------

loadXfromAC :: (Monad m) => Step m ()
loadXfromAC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (ac,y,flag,ac,count,pc,ir,addr,mem)
  incrementCount

loadYfromMEM :: (Monad m) => Step m ()
loadYfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x, mem ! addr ,flag,ac,count,pc,ir,addr,mem)
  incrementCount

loadYfromIR :: (Monad m) => Step m ()
loadYfromIR = do 
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,ir,flag,ac,count,pc,ir,addr,mem)
  incrementCount


loadACfromALU :: (Monad m) => Word16 -> Step m ()
loadACfromALU w = do 
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,w,count,pc,ir,addr,mem)
  incrementCount


loadACfromMEM :: (Monad m) => Step m ()
loadACfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,mem ! addr,count,pc,ir,addr,mem)
  incrementCount

loadMEMfromAC :: (Monad m) => Step m ()
loadMEMfromAC = do 
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let mem' = insert addr ac mem
  put (x,y,flag,ac,count,pc,ir,addr, mem' )
  incrementCount

loadACfromIR :: (Monad m) => Step m ()
loadACfromIR = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ir,count,pc,ir,addr,mem)
  incrementCount

incrementAC :: (Monad m) => Step m ()
incrementAC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag, ac+1 ,count,pc,ir,addr,mem)
  incrementCount


decrementAC :: (Monad m) => Step m ()
decrementAC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag, ac - 1 ,count,pc,ir,addr,mem)
  incrementCount

loadFLAGfromALU :: (Monad m) => Word1 -> Step m ()
loadFLAGfromALU b = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,b,ac,count,pc,ir,addr,mem)
  incrementCount

loadPCfromIR :: (Monad m) => Step m ()
loadPCfromIR = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,pc') = splitIR ir
  put (x,y,flag,ac,count, pc' ,ir,addr,mem)
  incrementCount


loadPCfromMEM :: (Monad m) => Step m ()
loadPCfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,pc') = splitIR $ mem ! addr
  put (x,y,flag,ac,count, pc' ,ir,addr,mem)
  incrementCount


incrementPC :: (Monad m) => Step m ()
incrementPC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,count, pc + 1,ir,addr,mem)
  incrementCount


loadIRfromMEM :: (Monad m) => Step m ()
loadIRfromMEM = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,count,pc, mem ! addr ,addr,mem)
  incrementCount


loadADDRfromPC :: (Monad m) => Step m ()
loadADDRfromPC = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  put (x,y,flag,ac,count,pc,ir,pc,mem)
  incrementCount


loadADDRfromIR :: (Monad m) => Step m ()
loadADDRfromIR = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,addr') = splitIR ir
  put (x,y,flag,ac,count,pc,ir, addr' ,mem)
  incrementCount

loadADDRfromY :: (Monad m) => Step m ()
loadADDRfromY = do
  (x,y,flag,ac,count,pc,ir,addr,mem) <- get
  let (_,addr') = splitIR y
  put (x,y,flag,ac,count,pc,ir, addr' ,mem)
  incrementCount

---------------------------------------------------------
---------------------------------------------------------

-- needs refactoring
aluADD :: (Monad m) => Step m (Word1,Word16)
aluADD = do s <- get
            let x = pX s
            let y = pY s
            incrementCount
            return (carry x y, x + y)
            
  where carry x y = if (x + y < x && x + y < y) then 1 else 0

aluSUB :: (Monad m) => Step m (Word1,Word16)
aluSUB = do s <- get
            let x = pX s
            let y = pY s
            incrementCount
            return (flag x y, x - y)
  where flag x y = if (x < y) then 1 else 0 

aluAND :: (Monad m) => Step m (Word1,Word16)
aluAND = do s <- get
            let x = pX s
            let y = pY s
            incrementCount
            return (0, x .&. y)

aluOR :: (Monad m) => Step m (Word1,Word16)
aluOR = do s <- get
           let x = pX s
           let y = pY s
           incrementCount
           return (0, x .|. y)

aluNOT :: (Monad m) => Step m (Word1,Word16)
aluNOT = do s <- get
            let x = pX s
            incrementCount
            return (0,complement x)

aluSHL :: (Monad m) => Step m (Word1,Word16)
aluSHL = do s <- get
            let x = pX s
            incrementCount
            let bits    = testBit x <$> [15,14..0]
            let flag    = if (head bits) then 1 else 0
            let shifted = sum $ convert <$> (zip [15,14..1] $ tail bits)
            return (flag,shifted)
  where convert(i,b) = if b then 2^i else 0


aluSHR :: (Monad m) => Step m (Word1,Word16)
aluSHR = do s <- get
            let x = pX s
            incrementCount
            let bits = testBit x <$> [15,14..0]
            let flag = if (last bits) then 1 else 0
            let shifted = sum $ convert <$> (zip [14,13..0] $ init bits)
            return (flag,shifted)
  where convert(i,b) = if b then 2^i else 0

acZero :: (Monad m) => Step m Bool
acZero = do s <- get
            let ac = pAC s
            incrementCount
            return (ac == 0)


acNegative :: (Monad m) => Step m Bool
acNegative = do s <- get
                let ac = pAC s
                incrementCount
                return $ testBit ac 15

flagSet :: (Monad m) => Step m Bool
flagSet = do s <- get
             let flag = pFLAG s
             return (flag == 1)

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
