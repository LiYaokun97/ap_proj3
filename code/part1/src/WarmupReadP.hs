module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--  E  ::= T E1| "-" T E1
--  E1 ::= "+" T E1 | "-" T E1 | ε
--  T  ::= num | "(" E ")" 

import Text.ParserCombinators.ReadP
    ( string, munch, satisfy, char, (<++), readP_to_S, ReadP, skipSpaces )
  -- may use instead of +++ for easier portability to Parsec
import Data.Char

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S pProgram s of
  [] -> Left "error : empty parsing"
  x -> case x of
    [(e, "")] -> Right e
    others -> Left $ "error : can not parse the string " ++ show others

pProgram :: Parser Exp
pProgram = do 
  skipSpaces
  pExp

--  E  ::= T E1| "-" T E1
pExp :: Parser Exp
pExp =
    do
      t <- pTerm
      pExp1 t
  <++
    do 
      char '-'
      skipSpaces
      t <- pTerm
      pExp1 (Negate t)


-- E1 ::= "+" T E1 | "-" T E1 | ε
pExp1 :: Exp -> Parser Exp
pExp1 exp =
    do
      char '+'
      skipSpaces
      t <- pTerm
      pExp1 (Add exp t)
  <++
    do
      char '-'
      skipSpaces
      t <- pTerm
      pExp1 (Add exp (Negate t) )
  <++
    do
      skipSpaces 
      return exp 


-- T  ::= num | "(" E ")"
pTerm :: Parser Exp
pTerm =
    do
      pNum
  <++
    do
      char '('
      skipSpaces
      exp <- pExp
      char ')'
      skipSpaces
      return exp

-- the first digit of a number shouldn't be 0 unless the number is 0
pInteger ::  Parser Exp
pInteger = do
         d  <- satisfy (\v -> v/= '0' && isDigit v)
         ds <- munch isDigit
         return (Num (read (d:ds) :: Int))

pNum::Parser Exp
pNum = 
      do 
        string "0"
        return $Num 0 
    <++
      do 
        exp <- pInteger
        skipSpaces
        return exp 

