
module BoaParser where

import BoaAST
import Text.ParserCombinators.ReadP
import Data.Char
import GHC.Float (fromRat'')

stringConst :: ReadP Exp
stringConst = between  (char '(')
                    (char ')')
                    convertString 


convertString :: ReadP Exp
convertString =  do
    s <- munch1 (\x-> x/=')' && isAscii  x)
    return (Const (StringVal s))


test2 = many (satisfy (> 'A')) >> string "cd"



pString = do 
            s <- between (char '\'') (char '\'') (pStringCheck "") 
            return $ Const $ StringVal s


type Parser x = ReadP x


pPrintAbleChar :: Parser Char
pPrintAbleChar = satisfy (\v -> isPrint v && v /= '\\' && v /= '\'' && v /= '\n')

pStringCheck :: String -> Parser String
pStringCheck s = do
                    char '\\'
                    char 'n'
                    pStringCheck $ s ++ "\n"
               <++
                  do
                    char '\\'
                    char '\\'
                    pStringCheck $ s ++ "\\"
               <++
                  do
                    char '\\'
                    char '\''
                    pStringCheck $ s ++ "'"
               <++
                  do
                    char '\\'
                    char '\n'
                    pStringCheck s
               <++
                  do
                    char '\\'
                    satisfy (\x-> isPrint x && x/='\\' && x/='\'' && x/= '\n' && x/= 'n')
                    return pfail "back!!!"
               <++
                  do
                    a <- satisfy (\x -> isAscii x && x /= '\'')
                    pStringCheck $ s ++ [a]               
                <++ 
                  do
                    string ""
                    return s


pStringTest :: Parser String
pStringTest = do 
                satisfy isAscii
                char 'b'
                return "aby"
              +++
                do
                    satisfy (\a -> a=='a')
                    char 'b'
                    return "abx"

pString2 = do
             char '\''
             r <- pStringCheck2 ""
             return $ Const $ StringVal r

pStringCheck2 :: String -> Parser String
pStringCheck2 s = do
                    char '\\'
                    char 'n'
                    pStringCheck2 $ s ++ "\n"
               <++
                  do
                    char '\\'
                    char '\\'
                    pStringCheck2 $ s ++ "\\"
               <++
                  do
                    char '\\'
                    char '\''
                    pStringCheck2 $ s ++ "'"
               <++
                  do
                    char '\\'
                    char '\n'
                    pStringCheck2 s
               <++
                  do
                    a <- satisfy (\v -> isPrint v && v /= '\\' && v /= '\'' && v /= '\n')
                    pStringCheck2 $ s ++ [a]
               <++
                  do
                    char '\''
                    return s


skipSpacesTest :: String -> Parser String
skipSpacesTest s = do
                    char 'x'
                    char 'y'
                    skipSpacesTest (s ++ "xy")
                <++
                    do
                        x <- satisfy isPrint
                        return [x]


skipSpaceAndComment :: Parser()
skipSpaceAndComment = do
   skipSpaces
   s <- look
   if s /="" && head s == '#' then 
      do 
         between (char '#') (char '\n') (munch (\x -> x/='\n' && isPrint x))
         skipSpaceAndComment
   else return ()
-- x = 'Just \\n a String \\\\ without others' #sadfewf \n #safwefwfsdf 