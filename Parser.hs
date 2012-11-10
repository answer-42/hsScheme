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
integer = (Number . read) <$> digits

float :: Parser LispVal
float = do
  beforePoint <- digits
  P.char '.'
  afterPoint <- digits
  return $ Float $ read (beforePoint ++ "." ++ afterPoint)

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
parseExpr =  Parser.string
        <|> symbol
        <|> char
        <|> float
        <|> integer
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

                
