{-# LANGUAGE BinaryLiterals #-}

module AssemblerTranslator (produceMemoryMap) where

import Machine (Instruction(..))
import Control.Monad.State.Lazy
import AssemblerParser (Operand(..), ASM(..))
import Data.Word.Odd
import Data.Map


type FocusList = ([(Word10,ASM)], [ASM])
type LabelTable = Map Operand Word10
type TranslationState = (Word10,FocusList,LabelTable)
type Translator m = StateT TranslationState m

initial :: [ASM] -> Word10 -> TranslationState
initial p w = (w,([],p),empty)

-- unsafe
moveRight' :: (Monad m) => Translator m ()
moveRight' = do (w,(l,(r:rs)),m) <- get
                case r of Op _ _     -> put (w+1,(l ++ [(w,r)],rs),m)
                          LabelDef s -> put (w,(l,rs), insert (Label s) w m)
                          ASMdata    -> put (w+1,(l++[(w,r)],rs),m)

moveToEnd :: (Monad m) => Translator m ()
moveToEnd = do (w,(l,r),m) <- get
               case r of [] -> return ()
                         _  -> moveRight' >> moveToEnd

--produceMemoryMap :: [ASM] -> Word10 -> [(Word10,Word16)]
produceMemoryMap p s = [ (k, translateASM a) | (k,a) <- l ]
  where (e,(l,r),m) = execState moveToEnd $ initial p s
        t o = case o of Label word -> m ! o -- translateOperand
                        Lit n      -> toEnum n

        translateASM ASMdata = 0
        translateASM (Op i y) | i == ADD  = g 0b000000 y
                              | i == SUB  = g 0b000001 y
                              | i == AND  = g 0b000010 y
                              | i == OR   = g 0b000011 y
                              | i == NOT  = g 0b000100 y
                              | i == SHL  = g 0b000101 y
                              | i == SHR  = g 0b000110 y
                              | i == INC  = g 0b000111 y
                              | i == DEC  = g 0b001000 y
                              | i == LOD  = g 0b001001 y
                              | i == STO  = g 0b001010 y
                              | i == HLT  = g 0b001011 y
                              | i == JMP  = g 0b001100 y
                              | i == JMZ  = g 0b001101 y
                              | i == JMN  = g 0b001110 y
                              | i == JMF  = g 0b001111 y
                              | i == ADDC = g 0b010000 y
                              | i == SUBC = g 0b010001 y
                              | i == ANDC = g 0b010010 y
                              | i == ORC  = g 0b010011 y
                              | i == LODC = g 0b011001 y
                              | i == ADDI = g 0b100000 y
                              | i == SUBI = g 0b100001 y
                              | i == ANDI = g 0b100010 y
                              | i == ORI  = g 0b100011 y
                              | i == LODI = g 0b101001 y
                              | i == STOI = g 0b101010 y
                              | i == JMPI = g 0b101100 y
                              | i == JMZI = g 0b101101 y
                              | i == JMNI = g 0b101110 y
                              | i == JMFI = g 0b101111 y
        f = 2^10
        g n y = case y of Just o  -> n * f + (t o)
                          Nothing -> n * f
