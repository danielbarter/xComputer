{-# LANGUAGE BinaryLiterals #-}

module Machine (
               X,
               Y,
               FLAG,
               AC,
               COUNT,
               PC,
               IR,
               ADDR,
               MEM,
               Instruction(..),
               XComputerState,
               Step,
               cycle,
               word10toword16
               ) where

import Prelude hiding (cycle)
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



-- split a 16bit number into the top 6 bits and the lower 10 bits.
-- there must be a better way to do this
splitIR :: Word16 -> (Word6,Word10)
splitIR ir = (sum $ convert <$> (zip [5,4..0] bits6), sum $ convert <$> (zip [9,8..0] bits10) )
  where (bits6,bits10) = splitAt 6 $ testBit ir <$> [15,14..0]
        convert (i,b) = if b then 2^i else 0

-- convert a 10bit number to a 16bit number
-- there must be a better way to do this
word10toword16 :: Word10 -> Word16
word10toword16 w = sum $ convert  <$> zip [9,8..0] (testBit w <$> [9,8..0])
  where convert (i,b) = if b then 2^i else 0


-- main memory
type MEM = Map Word10 Word16

-- instruction codes are 6 bits
type InstructionCode = Word6


data Instruction = ADD    -- 0b000000
                 | SUB    -- 0b000001
                 | AND    -- 0b000010
                 | OR     -- 0b000011
                 | NOT    -- 0b000100
                 | SHL    -- 0b000101
                 | SHR    -- 0b000110
                 | INC    -- 0b000111
                 | DEC    -- 0b001000
                 | LOD    -- 0b001001
                 | STO    -- 0b001010
                 | HLT    -- 0b001011
                 | JMP    -- 0b001100
                 | JMZ    -- 0b001101
                 | JMN    -- 0b001110
                 | JMF    -- 0b001111

                 | ADDC   -- 0b010000
                 | SUBC   -- 0b010001
                 | ANDC   -- 0b010010
                 | ORC    -- 0b010011
                 | LODC   -- 0b011001

                 | ADDI   -- 0b100000
                 | SUBI   -- 0b100001
                 | ANDI   -- 0b100010
                 | ORI    -- 0b100011
                 | LODI   -- 0b101001
                 | STOI   -- 0b101010
                 | JMPI   -- 0b101100
                 | JMZI   -- 0b101101
                 | JMNI   -- 0b101110
                 | JMFI   -- 0b101111
                      deriving (Show, Eq, Read)

translate :: Word6 -> Instruction
translate n = case n of 0b000000  -> ADD
                        0b000001  -> SUB
                        0b000010  -> AND
                        0b000011  -> OR
                        0b000100  -> NOT
                        0b000101  -> SHL
                        0b000110  -> SHR
                        0b000111  -> INC
                        0b001000  -> DEC
                        0b001001  -> LOD
                        0b001010  -> STO
                        0b001011  -> HLT
                        0b001100  -> JMP
                        0b001101  -> JMZ
                        0b001110  -> JMN
                        0b001111  -> JMF

                        0b010000  -> ADDC
                        0b010001  -> SUBC
                        0b010010  -> ANDC
                        0b010011  -> ORC
                        0b011001  -> LODC

                        0b100000  -> ADDI
                        0b100001  -> SUBI
                        0b100010  -> ANDI
                        0b100011  -> ORI
                        0b101001  -> LODI
                        0b101010  -> STOI
                        0b101100  -> JMPI
                        0b101101  -> JMZI
                        0b101110  -> JMNI
                        0b101111  -> JMFI
                        
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

execute :: (Monad m) => Instruction -> Step m Instruction
execute i = case i of ADD ->  addressMode00  aluADD
                      SUB ->  addressMode00  aluSUB
                      AND ->  addressMode00  aluAND
                      OR  ->  addressMode00  aluOR
                      NOT ->  addressMode00' aluNOT
                      SHL ->  addressMode00' aluSHL
                      SHR ->  addressMode00' aluSHR

                      INC ->  do incrementAC
                                 return i

                      DEC ->  do decrementAC
                                 return i

                      LOD ->  do loadADDRfromIR
                                 loadACfromMEM
                                 return i

                      STO ->  do loadADDRfromIR
                                 loadMEMfromAC
                                 return i

                      HLT ->  do incrementCount 
                                 return i

                      JMP ->  do loadPCfromIR
                                 return i

                      JMZ ->  do b <- acZero
                                 if b then loadPCfromIR >> (return i) else (return i)

                      JMN ->  do b <- acNegative
                                 if b then loadPCfromIR >> (return i) else (return i)

                      JMF ->  do b <- flagSet
                                 if b then loadPCfromIR >> (return i) else (return i)

                      ADDC -> addressMode01 aluADD
                      SUBC -> addressMode01 aluSUB
                      ANDC -> addressMode01 aluAND
                      ORC  -> addressMode01 aluOR

                      LODC -> do loadACfromIR
                                 return i

                      ADDI -> addressMode10 aluADD
                      SUBI -> addressMode10 aluSUB
                      ANDI -> addressMode10 aluAND
                      ORI  -> addressMode10 aluOR

                      LODI -> do addressMode10'
                                 loadACfromMEM
                                 return i

                      STOI -> do addressMode10'
                                 loadMEMfromAC
                                 return i

                      JMPI -> do addressMode10'
                                 loadPCfromMEM
                                 return i

                      JMZI -> do b <- acZero
                                 if b then do addressMode10'
                                              loadPCfromMEM
                                              return i
                                      else return i

                      JMNI -> do b <- acNegative
                                 if b then do addressMode10'
                                              loadPCfromMEM
                                              return i
                                      else return i

                      JMFI -> do b <- flagSet
                                 if b then do addressMode10'
                                              loadPCfromMEM
                                              return i
                                      else return i

  where addressMode00 alu  = do loadADDRfromIR
                                loadXfromAC
                                loadYfromMEM
                                (flag,ac) <- alu
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i
        addressMode00' alu = do loadXfromAC
                                (flag,ac) <- alu
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i
        addressMode01 alu  = do loadXfromAC
                                loadYfromIR
                                (flag,ac) <- alu
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i
        addressMode10 alu  = do loadADDRfromIR
                                loadYfromMEM
                                loadADDRfromY
                                loadXfromAC
                                loadYfromMEM
                                (flag,ac) <- alu
                                loadACfromALU ac
                                loadFLAGfromALU flag
                                return i
        addressMode10'     = do loadADDRfromIR
                                loadYfromMEM
                                loadADDRfromY


cycle :: (Monad m) => Step m Instruction
cycle = setCountToZero >> fetch >>= execute

-------------------------------------------------
-------------------------------------------------

incrementCount :: (Monad m) => Step m ()
incrementCount = modify $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac, count + 1 ,pc,ir,addr,mem)


setCountToZero :: (Monad m) => Step m ()
setCountToZero = modify $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,0,pc,ir,addr,mem)


-------------------------------------------------
-------------------------------------------------

dataMovement f = (modify f) >> incrementCount

loadXfromAC :: (Monad m) => Step m ()
loadXfromAC = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (ac,y,flag,ac,count,pc,ir,addr,mem)

loadYfromMEM :: (Monad m) => Step m ()
loadYfromMEM = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x, mem ! addr ,flag,ac,count,pc,ir,addr,mem)

loadYfromIR :: (Monad m) => Step m ()
loadYfromIR = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x, (word10toword16 . snd . splitIR) ir  ,flag,ac,count,pc,ir,addr,mem)

loadACfromALU :: (Monad m) => Word16 -> Step m ()
loadACfromALU w = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,w,count,pc,ir,addr,mem)

loadACfromMEM :: (Monad m) => Step m ()
loadACfromMEM = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,mem ! addr,count,pc,ir,addr,mem) 

loadMEMfromAC :: (Monad m) => Step m ()
loadMEMfromAC = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count,pc,ir,addr, insert addr ac mem)

loadACfromIR :: (Monad m) => Step m ()
loadACfromIR = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag, (word10toword16 . snd . splitIR) ir  ,count,pc,ir,addr,mem)

incrementAC :: (Monad m) => Step m ()
incrementAC = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag, ac+1 ,count,pc,ir,addr,mem)

decrementAC :: (Monad m) => Step m ()
decrementAC = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag, ac - 1 ,count,pc,ir,addr,mem)

loadFLAGfromALU :: (Monad m) => Word1 -> Step m ()
loadFLAGfromALU b = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,b,ac,count,pc,ir,addr,mem)

loadPCfromIR :: (Monad m) => Step m ()
loadPCfromIR = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count, snd $ splitIR ir ,ir,addr,mem)

loadPCfromMEM :: (Monad m) => Step m ()
loadPCfromMEM = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count, snd $ splitIR $ mem ! addr ,ir,addr,mem)

incrementPC :: (Monad m) => Step m ()
incrementPC = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count, pc + 1,ir,addr,mem)

loadIRfromMEM :: (Monad m) => Step m ()
loadIRfromMEM = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count,pc, mem ! addr ,addr,mem)

loadADDRfromPC :: (Monad m) => Step m ()
loadADDRfromPC = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count,pc,ir,pc,mem)

loadADDRfromIR :: (Monad m) => Step m ()
loadADDRfromIR = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count,pc,ir, snd $ splitIR ir ,mem)

loadADDRfromY :: (Monad m) => Step m ()
loadADDRfromY = dataMovement $ \(x,y,flag,ac,count,pc,ir,addr,mem) -> (x,y,flag,ac,count,pc,ir, snd $ splitIR y ,mem)

---------------------------------------------------------
---------------------------------------------------------

alu flag ac = do s <- get
                 let x = pX s
                 let y = pY s
                 incrementCount
                 return (flag x y, ac x y)


aluADD :: (Monad m) => Step m (Word1,Word16)
aluADD = alu flag (+)
  where flag x y = if (x + y < x && x + y < y) then 1 else 0

aluSUB :: (Monad m) => Step m (Word1,Word16)
aluSUB = alu flag (-)
  where flag x y = if (x < y) then 1 else 0 

aluAND :: (Monad m) => Step m (Word1,Word16)
aluAND = alu (\x y -> 0) (.&.)

aluOR :: (Monad m) => Step m (Word1,Word16)
aluOR = alu (\x y -> 0) (.|.)

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
