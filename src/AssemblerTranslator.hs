module AssemblerTranslator (produceMemoryMap) where

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
produceMemoryMap p s = l
  where (e,(l,r),m) = execState moveToEnd $ initial p s
        t o = case o of Label word -> m ! word -- translateOperand
                        Lit n      -> n

        translateASM (Op i o) | i == ADD = 0b000000 * f + (t o)
                              | i == SUB = 0b000001 * f + (t o)
                              | i == AND = 0b000010 * f + (t o)
                              | i == OR  = 0b000011 * f + (t o)
                              | i == NOT = 0b000100 * f + (t o)
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
        f = 2^10
