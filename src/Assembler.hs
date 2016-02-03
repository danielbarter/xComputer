module Assembler (
                 assemblerParser
                 ) where

import Text.Parsec
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as T
import Machine (Instruction)

-- instructions are uppercase
-- label references are lower case
-- label definitions are .lowercase

data ASM = Op Instruction | Label String | LabelDef String | Lit Int | ASMdata
  deriving Show

assemblerSyntax :: T.LanguageDef ()
assemblerSyntax = T.LanguageDef {
  T.commentStart    = "{",
  T.commentEnd      = "}",
  T.commentLine     = "//",
  T.nestedComments  = False,
  T.identStart      = C.lower <|> (C.char '.'),
  T.identLetter     = C.lower,
  T.opStart         = C.upper,
  T.opLetter        = C.upper,
  T.reservedNames   = [],
  T.reservedOpNames = ["DATA"],
  T.caseSensitive   = True
  }

handleLabel s = if (head s == '.') then LabelDef $ tail s
                             else Label s


assemblerLexer = T.makeTokenParser assemblerSyntax

identifier = handleLabel      <$> T.identifier assemblerLexer
operator   = (Op . read)      <$> T.operator   assemblerLexer
whiteSpace = T.whiteSpace                      assemblerLexer
natural    = (Lit . fromEnum) <$> T.natural    assemblerLexer
reservedOp = (\x -> ASMdata)  <$> (T.reservedOp assemblerLexer $ "DATA")

assemblerParser = do whiteSpace
                     list <- many (identifier <|> operator <|> natural <|> reservedOp)
                     eof
                     return list
