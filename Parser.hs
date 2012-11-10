-- file: parser.hs

module Parser where

import Control.Applicative ((<$>), (*>), (<*>))
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

import Ast

string :: Parser LispVal
string = String <$> between doubleQuote doubleQuote (many (escaped <|> noneOf "\"\\"))
  where doubleQuote = char '"'
        escaped = char '\\'
                *> (fromJust
               <$> flip lookup specialChars
               <$> oneOf "\\\"nrt")
        specialChars = [('\\', '\\'), ('"', '"'), ('n', '\n'),
                        ('r', '\r'), ('t', '\t')]
                  
