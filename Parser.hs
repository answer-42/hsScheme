-- file: parser.hs

module Parser where

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (char, string)
import qualified Text.ParserCombinators.Parsec as P

import Ast

char :: Parser LispVal
char = Char <$> (P.string "#\\" *> letter)

string :: Parser LispVal
string = String <$> between doubleQuote doubleQuote (many (escaped <|> noneOf "\"\\"))
  where doubleQuote = P.char '"'
        escaped = P.char '\\'
               *> (fromJust <$> flip lookup specialChars
                            <$> oneOf "\\\"nrt")
        specialChars = [('\\', '\\'), ('"', '"'), ('n', '\n'),
                        ('r', '\r'), ('t', '\t')]

symbol :: Parser LispVal
symbol = Symbol <$> ((:) <$> letter <*> many (letter <|> digit))

number :: Parser LispVal
number = Number . read <$> many1 digit

float :: Parser LispVal
float = Float
    <$> ((read .) . (++) <$> many digit
                         <*> (P.char '.' *> many1 digit))

expression :: Parser LispVal
expression = try char
         <|> try string
         <|> try symbol
         <|> try float
         <|> number
         
parseExpr :: String -> String
parseExpr s = case parse expression "scheme" s of
  Left e -> "error: " ++ show e
  Right a -> case a of
    Char c -> "char: " ++ [c]
    String s -> "string: " ++ s
    Symbol s -> "symbol: " ++ s
    Number n -> "number: " ++ show n
    Float r -> "float: " ++ show r
  
-- tests

test = mapM_ (putStrLn . parseExpr) tests
  where tests = ["#\\a", "\"hello \\\"scheme\\\"\"",
                 "123", "1.42", "symbol42"]


                
