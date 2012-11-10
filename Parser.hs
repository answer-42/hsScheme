-- file: parser.hs

module Parser where

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (char)
import qualified Text.ParserCombinators.Parsec as P

import Ast

digits = many1 digit

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

integer :: Parser LispVal
integer = (Number . read) <$> digits

float :: Parser LispVal
float = do
  beforePoint <- digits
  P.char '.'
  afterPoint <- digits
  return $ Float $ read (beforePoint ++ "." ++ afterPoint)

symbol :: Parser LispVal
symbol = Symbol <$> ((:) <$> letter <*> many (letter <|> digit))

number :: Parser LispVal
number = Number . read <$> many1 digit
 
parseExpr :: Parser LispVal
parseExpr = Parser.string
        <|> float
        <|> integer

readExpr :: String -> Either ParseError LispVal
readExpr input = parse parseExpr "test" input

                
