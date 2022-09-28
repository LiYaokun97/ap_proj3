{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use unless" #-}
-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Set as Set

type ParseError = String -- you may replace this
type Parser a = ReadP a
parseString :: String -> Either ParseError Program
parseString s = case readP_to_S pProgram s of
                   [] -> Left "empty parsing"
                   x -> case x of
                           [(e,"")] -> Right e
                           ls -> Left ("z" ++ show ls)

-- use set, so when query, the complexity is O(nlgn)
keywords :: Set.Set String
keywords = Set.fromList["None", "True", "False", "for", "if", "in", "not"]


pProgram :: Parser Program
pProgram = do skipSpaceAndComment
              pStmts


--   Stmts ::= Stmt';'Stmts| Stmt
pStmts :: Parser [Stmt]
pStmts =  do
             n <- pStmt
             symbol ";"
             m <- pStmts
             return $ n:m
       <++
          do n <- pStmt
             return [n]

--   Stmt ::= ident '=' Exp1| Exp1
pStmt :: Parser Stmt
pStmt =  do
            var <- pIdent
            symbol "="
            SDef var <$> pExp1
      <++
         do SExp <$> pExp1
{-
In python, the priority of "not" is lower than "+" "-" and "*" "//".
So when rewriting the grammer, "not" should be handled before "+" and "*"  
Exp1 ::= 'not' Exp1 | Exp2
-}
pExp1 :: Parser Exp
pExp1  = do
           symbol "not"
           Not <$> pExp1
       <++
           pExp2

--   Exp2 ::= Exp3 opExp2
pExp2 :: Parser Exp
pExp2  = do a <- pExp3
            opExp2 a


--   opExp2 ::= ε | '==' Exp3 | '<' Exp3 | '>' Exp3 | 'in' Exp3 | 'not' 'in' Exp3 | '<=' Exp3 | '>=' Exp3 | '!=' Exp3
opExp2 :: Exp -> Parser Exp
opExp2 a= do symbol "=="
             Oper Eq a <$> pExp3
       <++
          do  symbol "<"
              Oper Less a <$> pExp3
       <++
          do  symbol ">"
              Oper Greater a <$> pExp3
       <++
          do  symbol "in"
              Oper In a <$> pExp3
       <++
          do  symbol "not"
              symbol "in"
              Not . Oper In a <$> pExp3
       <++
          do  symbol "<="
              Not . Oper Greater a <$> pExp3
       <++
          do  symbol ">="
              Not . Oper Less a<$> pExp3
       <++
          do  symbol "!="
              Not . Oper Eq a <$> pExp3
       <++
          return a


pExp3 :: Parser Exp
pExp3  = do a <- pExp4
            plusExp  a


-- for plus and minus
plusExp :: Exp -> Parser Exp
plusExp a = do
             symbol "+"
             b<- pExp4
             plusExp ( Oper Plus a b )
        <++
           do
             symbol "-"
             b<- pExp4
             plusExp ( Oper Minus a b )
        <++
           return a


pExp4 :: Parser Exp
pExp4  = do a <- pTerm
            timesExp a


-- for "*" "//" "%"
timesExp :: Exp -> Parser Exp
timesExp a = do
             symbol "*"
             b<- pTerm
             timesExp $ Oper Times a b
        <++
           do
             symbol "//"
             b<- pTerm
             timesExp $ Oper Div a b
        <++
           do
             symbol "%"
             b<- pTerm
             timesExp $ Oper Mod a b
        <++
           return a

pTerm:: Parser Exp
pTerm = do pNumSkipAll
        <++ do pParenthesesExp
        <++ do pCall
        <++ do pCompr
        <++ do pVar
        <++ do pList
        <++ do pString
        <++ do pTrue
        <++ do pFalse
        <++ do pNone



pParenthesesExp :: Parser Exp
pParenthesesExp = between (symbol "(") (symbol ")") pExp1

-- ident   
pVar :: Parser Exp
pVar = do
        Var <$> pIdent

-- the first digit of a number shouldn't be 0 unless the number is 0
pInteger :: Bool -> Parser Exp
pInteger isPos = do
         d  <- satisfy (\v -> v/= '0' && isDigit v)
         ds <- munch isDigit
         let num = if isPos then d:ds else '-':d:ds
         return (Const (IntVal (read num :: Int)))

pNum::Parser Exp
pNum = do
         char '-'
         pInteger False
    <++
      do pInteger True
    <++
      do string  "-0"
         return (Const (IntVal 0))
    <++
      do char '0'
         return (Const (IntVal 0))

pNumSkipAll :: Parser Exp
pNumSkipAll = do
               exp <- pNum
               skipSpaceAndComment
               return exp


pString = do
            s <- between (char '\'') (char '\'') (pStringHelper "")
            skipSpaceAndComment
            return $ Const $ StringVal s

pStringHelper :: String -> Parser String
pStringHelper s = do
                    a <- satisfy (\x -> isAscii x && x /= '\'' && x /= '\\' && x /= '\n')
                    pStringHelper $ s ++ [a]
               <++ do
                    string "\\n"
                    pStringHelper $ s ++ "\n"
               <++
                  do
                    string "\\\\"
                    pStringHelper $ s ++ "\\"
               <++
                  do
                    string "\\\'"
                    pStringHelper $ s ++ "'"
               <++
                  do
                    string "\\\n"
                    pStringHelper s
               <++
                  do
                    char '\\'
                    satisfy (\x-> isPrint x && x/='\\' && x/='\'' && x/= '\n' && x/= 'n')
                    return pfail "a backslash can only follow by a single quote, blackslash, letter n or a newline"
               <++
                  return s


pIdent :: Parser String
pIdent = lexeme $ do
            a <- satisfy (\x ->  x =='_' || isLetter x )
            as <- munch (\x -> x =='_' || isLetter x || isDigit x)
            let n = a : as
            if not (Set.member n keywords) then return n
                        else return pfail "indentity can not be the same as keyword"


pList :: Parser Exp
pList =  do
           symbol "["
           expz <- pExpz
           symbol "]"
           return $ List expz


pExpz :: Parser [Exp]
pExpz = do
           pExps
       <++ return []


pExps :: Parser [Exp]
pExps = do
          e <- pExp1
          symbol ","
          es <- pExps
          return (e:es)
     <++
        do
          e <- pExp1
          return  [e]

pCall :: Parser Exp
pCall = do
           a <- pIdent
           symbol "("
           e<- pExpz
           symbol ")"
           return $ Call a e

pCompr :: Parser Exp
pCompr = do
           symbol "["
           e <- pExp1
           for <- pFor
           other <- pClause
           symbol "]"
           return $ Compr e (for ++ other)

--   Clausez ::= ε | ForClause Clausez | IfClause Clausez
pClause:: Parser [CClause]
pClause= do e <- pFor
            es <- pClause
            return $ e ++ es
      <++
         do e <- pIf
            es <- pClause
            return $ e ++ es
      <++
         do skipSpaceAndComment
            return []


pFor :: Parser [CClause]
pFor = do
           symbol "for"
           name <- pIdent
           symbol "in"
           e <- pExp1
           return  [CCFor name e]


pIf :: Parser [CClause]
pIf = do
           symbol "if"
           e <- pExp1
           return  [CCIf e]


pNone::Parser Exp
pNone = do symbol "None"
           return $ Const NoneVal

pTrue::Parser Exp
pTrue = do symbol "True"
           return $ Const TrueVal

pFalse::Parser Exp
pFalse = do symbol "False"
            return $ Const FalseVal

{-
when getting a simple, handle the following space or comment at the same time,
then we don't need to consider space or comment when dealing with next expression.
-}
symbol:: String ->Parser ()
symbol s = do
              string s
              skipSpaceAndComment
              return ()

lexeme:: Parser a -> Parser a
lexeme p = do
             a <- p
             skipSpaceAndComment
             return a


{- 
Because Boa is not indentation-sensitive, there may be newline or even comment 
after ident or keywords, just skip them. 
-}

skipSpaceAndComment :: Parser()
skipSpaceAndComment = do
   skipSpaces
   s <- look
   if s /="" && head s == '#' then
      do
         munch1 (/= '\n')
         skipSpaceAndComment
   else return ()
