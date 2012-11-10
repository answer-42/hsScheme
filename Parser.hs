-- file: parser.hs

module Parser where

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (char)
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
    <$> ((+) <$> (read <$> many digit)
             <*  P.char '.'
              *> (read . ("0." ++) <$> many1 digit))
                  
