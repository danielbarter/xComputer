module AssemblerParser (
                 assemblerParser,
                 Operand(..),
                 ASM(..)
                 ) where

import Text.Parsec
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as T
import Machine (Instruction)

-- instructions are uppercase
-- label references are lower case
-- label definitions are .lowercase

data Operand = Label String | Lit Int deriving (Eq, Show, Ord)
data ASM = Op Instruction (Maybe Operand) | LabelDef String | ASMdata deriving Show

assemblerSyntax :: T.LanguageDef ()
assemblerSyntax = T.LanguageDef {
  T.commentStart    = "{",
  T.commentEnd      = "}",
  T.commentLine     = "//",
  T.nestedComments  = False,
  T.identStart      = C.lower,
  T.identLetter     = C.lower,
  T.opStart         = C.upper <|> (char '.'),
  T.opLetter        = C.upper <|> C.lower,
  T.reservedNames   = [],
  T.reservedOpNames = [],
  T.caseSensitive   = True
  }



assemblerLexer = T.makeTokenParser assemblerSyntax

identifier = T.identifier assemblerLexer
operator   = T.operator   assemblerLexer
whiteSpace = T.whiteSpace assemblerLexer
number     = T.integer    assemblerLexer

parseOperator = do s <- operator
                   case s of ('.':l) -> return $ LabelDef l
                             "DATA"  -> return ASMdata
                             _       -> do o <- parseOperand
                                           return $ Op (read s) o

parseOperand = 
  foldr1 (<|>) [(Just . Label) <$> identifier, 
                (Just . Lit . fromInteger) <$> number,
                whiteSpace >> (return Nothing)
                ]


assemblerParser = do whiteSpace
                     list <- many parseOperator
                     eof
                     return list
