-- file: parser.hs

module Parser where

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (char, string)
import qualified Text.ParserCombinators.Parsec as P

import Ast

digits = many1 digit
lparen = P.char '('
rparen = P.char ')'

getSign :: (Num a) => Parser (a -> a)
getSign = option id (do s <- oneOf "+-"
                        return $ case s of 
                                 '-' -> negate 
                                 '+' -> id)

char :: Parser LispVal
char = Char <$> (P.string "#\\" *> letter)


-- TODO String and Symbol should be more abstract.
--      Furthermore, we I think we should put bool/char into symbol.
--      Because those three types are in Scheme almost the same.
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

integer :: Parser LispVal
integer = do
  s <- getSign
  x <- digits
  return $ Number $ s (read x)

float :: Parser LispVal
float = do
  s <- getSign
  beforePoint <- digits
  P.char '.'
  afterPoint <- digits
  return $ Float $ s (read (beforePoint ++ "." ++ afterPoint))

dottedList :: Parser LispVal
dottedList = do
  lparen
  firstPart <- endBy1 parseExpr spaces
  secondPart <- P.char '.' >> spaces >> parseExpr
  rparen
  return $ DottedList firstPart secondPart

list :: Parser LispVal
list = do 
  lparen
  elem <- sepBy parseExpr spaces 
  rparen
  return $ List elem

parseExpr :: Parser LispVal
parseExpr =  try Parser.string
        <|> try symbol
        <|> try char
        <|> try float
        <|> try integer
        <|> try list
        <|> dottedList

readExpr :: String -> Either ParseError LispVal
readExpr input = parse parseExpr "test" input

{-        
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
-}

                
