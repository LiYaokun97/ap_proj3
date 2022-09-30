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
import qualified Data.Map as Map

type ParseError = String -- you may replace this
type Parser a = ReadP a
parseString :: String -> Either ParseError Program
parseString s = case readP_to_S pProgram s of
                   [] -> Left "empty parsing!!!"
                   x -> case x of
                           [ (e, "")] -> Right e
                           ls -> Left (show ls)

-- use Map, so when query, the complexity is O(1)
keywords = Map.fromList[("None", 1), ("True",1), ("False",1), ("for",1), ("if",1), ("in",1), ("not",1)]

-- forceSpaceKeyWords = Map.fromList["for", "if", "in", "not"]

pProgram :: Parser Program
pProgram = do
              skipSpaceAndComment
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
            var <- pIndent
            symbol "="
            SDef var <$> pExpr
      <++
         do SExp <$> pExpr
{-
In python, the priority of "not" is lower than "+" "-" and "*" "//".
So when rewriting the grammer, "not" should be handled before "+" and "*"  
Exp1 ::= 'not' Exp1 | Exp2
-}
pExpr :: Parser Exp
pExpr  = do
           keywordsForceFollowSpace "not"
           Not <$> pExpr
       <++
           pExp2

--   Exp2 ::= Exp3 operationExp
pExp2 :: Parser Exp
pExp2  = do a <- pExpr3
            operationExp a


--   operationExp ::= ε | '==' Exp3 | '<' Exp3 | '>' Exp3 | 'in' Exp3 | 'not' 'in' Exp3 | '<=' Exp3 | '>=' Exp3 | '!=' Exp3
operationExp :: Exp -> Parser Exp
operationExp exp =
          do symbol "=="
             Oper Eq exp <$> pExpr3
       <++
          do  symbol "<"
              Oper Less exp <$> pExpr3
       <++
          do  symbol ">"
              Oper Greater exp <$> pExpr3
       <++
          do  keywordsForceFollowSpace "in"
              Oper In exp <$> pExpr3
       <++
          do  keywordsForceFollowSpace "not"
              keywordsForceFollowSpace "in"
              Not . Oper In exp <$> pExpr3
       <++
          do  symbol "<="
              Not . Oper Greater exp <$> pExpr3
       <++
          do  symbol ">="
              Not . Oper Less exp<$> pExpr3
       <++
          do  symbol "!="
              Not . Oper Eq exp <$> pExpr3
       <++
          return exp


pExpr3 :: Parser Exp
pExpr3  = do 
            exp <- pExpr4
            plusExp exp


-- for plus and minus
plusExp :: Exp -> Parser Exp
plusExp exp = do
             symbol "+"
             exp' <- pExpr4
             plusExp ( Oper Plus exp exp' )
        <++
           do
             symbol "-"
             exp' <- pExpr4
             plusExp ( Oper Minus exp exp' )
        <++
           return exp


pExpr4 :: Parser Exp
pExpr4  = do 
            a <- term
            timesExp a


-- for "*" "//" "%"
timesExp :: Exp -> Parser Exp
timesExp a = do
             symbol "*"
             b<- term
             timesExp $ Oper Times a b
        <++
           do
             symbol "//"
             b<- term
             timesExp $ Oper Div a b
        <++
           do
             symbol "%"
             b<- term
             timesExp $ Oper Mod a b
        <++
           return a

term:: Parser Exp
term = do pNumSkipAll
        <++ do pParenthesesExp
        <++ do pCall
        <++ do pCompr
        <++ do pList
        <++ do pVar
        <++ do pString
        <++ do pNone
        <++ do pTrue
        <++ do pFalse


pNone::Parser Exp
pNone = do symbol "None"
           return $ Const NoneVal

pTrue::Parser Exp
pTrue = do symbol "True"
           return $ Const TrueVal

pFalse::Parser Exp
pFalse = do symbol "False"
            return $ Const FalseVal

pParenthesesExp :: Parser Exp
pParenthesesExp = between (symbol "(") (symbol ")") pExpr

-- ident   
pVar :: Parser Exp
pVar = do Var <$> pIndent

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
                    a <- satisfy (\x -> isPrint x && x /= '\'' && x /= '\\' && x /= '\n')
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


pIndent :: Parser String
pIndent = do
            a <- satisfy (\x ->  x =='_' || isLetter x && isPrint x)
            as <- munch (\x -> x =='_' || isLetter x || isDigit x && isPrint x)
            skipSpaceAndComment
            let n = a : as
            case Map.lookup n keywords of
               Just _ -> return pfail "indentity can not be the same as keyword"
               Nothing -> return n
            
{-
   handling situation where there are more than a pair of brackets first, through recursive calls of pList. 
   By looking ahead, the program don't need to search the whole search tree. 
   It may seems not elegant, but greatly useful to reduce the complexity in situation like deep brackets.
-}
pList :: Parser Exp
pList = 
      do  
         symbol "["
         expz <- pList
         symbol "]"
         return $ List [expz]
   <++
      do
         expz <- between (symbol "[") (symbol "]") pExprz
         return $ List expz

pExprz :: Parser [Exp]
pExprz = do
           pExprs
       <++ return []


pExprs :: Parser [Exp]
pExprs = do
          e <- pExpr
          symbol ","
          es <- pExprs
          return (e:es)
     <++
        do
          e <- pExpr
          return  [e]

pCall :: Parser Exp
pCall = do
           a <- pIndent
           e<- between (symbol "(") (symbol ")") pExprz
           return $ Call a e

pCompr :: Parser Exp

pCompr =
         do
           symbol "["
           e <- pExpr
           for <- pFor
           other <- pClause
           symbol "]"
           return $ Compr e (for ++ other)

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
           name <- between forKeywords (keywordsForceFollowSpace "in") pIndent
           e <- pExpr
           return  [CCFor name e]


pIf :: Parser [CClause]
pIf = do
           keywordsForceFollowSpace "if"
           e <- pExpr
           return  [CCIf e]


{-
when getting a symbol, handle the following space or comment at the same time,
then we can guarantee there will be no space before next expression.
-}
symbol:: String ->Parser ()
symbol s = do
               string s
               skipSpaceAndComment
               return ()

{-
   keywords including "in" "not" "if", must be followed by one of them:
      1. "[" 
      2. space 
      3. "(" 
      4."#"
   so, have to use this function to parse these keywords.
-}
keywordsForceFollowSpace :: String -> Parser ()
keywordsForceFollowSpace s = do
   string s 
   x <- look
   if head x == '[' || isSpace (head x) || head x == '(' || head x == '#'
      then skipSpaceAndComment
      else return pfail "keywords can not be used as variable"

{-
   when it comes to "for", it can not be followed by "[", but only "(" "#" space
-}
forKeywords:: Parser()
forKeywords = do
   string "for"
   x <- look
   if isSpace (head x) || head x == '(' || head x == '#'
      then skipSpaceAndComment
      else return pfail "keywords can not be used as variable"

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
