-- file: parser.hs

module Parser.Parser where

import Control.Applicative ((<$>), (*>), (<*), (<*>), pure)
import Control.Monad (join)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (char, string)
import qualified Text.ParserCombinators.Parsec as P

import Parser.AST

-- TODO 
--  * Add comment

digits = many1 digit
lparen = P.char '('
rparen = P.char ')'
between2 = join between

sign :: (Num a) => Parser (a -> a)
sign = option id (do s <- oneOf "+-"
                     return $ case s of 
                       '-' -> negate 
                       '+' -> id)

char :: Parser LispVal
char = Char <$> (P.string "#\\" *> letter)

string :: Parser LispVal
string = String <$> between2 doubleQuote (many (escaped <|> noneOf "\"\\"))
  where doubleQuote = P.char '"'
        escaped = P.char '\\'
               *> (fromJust <$> flip lookup escapedChars
                            <$> oneOf "\\\"nrt")
        escapedChars = [('\\', '\\'), ('"', '"'), ('n', '\n'),
                        ('r', '\r'), ('t', '\t')]

symbol :: Parser LispVal
symbol = Symbol <$> (((:) <$> (letter <|> specialChar)
                          <*> many (letter <|> specialChar <|> digit)) 
                     <|> (pure <$> oneOf "+-") <|> P.string "...")
  where specialChar = oneOf "!$%&*+-./:<=>?@^_~"
--        peculiarIdent = (oneOf "+-") <|> P.string "..."

bool :: Parser LispVal
bool = do
  p <- P.char '#' *> oneOf "tf"
  return (Bool (case p of
                   't' -> True
                   'f' -> False))

integer :: Parser LispVal
integer = Number <$> (sign <*> (read <$> digits))

-- comment = P.char ';' >> 

-- float :: Parser LispVal
-- float = do
--   s <- sign
--   beforePoint <- digits
--   P.char '.'
--   afterPoint <- digits
--   return $ Float $ s (read (beforePoint ++ "." ++ afterPoint))

dottedList :: Parser LispVal
dottedList = do
  firstPart <- spaces >> expr `endBy1` spaces
  secondPart <- P.char '.' >> between spaces spaces expr
  return $ DottedList firstPart secondPart
 
list :: Parser LispVal
list = List <$> (spaces *> expr `endBy` spaces)

quote :: Parser LispVal
quote = createQuote <$> (P.char '\'' *> expr)

expr :: Parser LispVal
expr = try string
   <|> try symbol
   <|> try bool
   <|> try char
   -- <|> try float
   <|> try integer
   <|> try quote
   <|> between lparen rparen (try dottedList <|> list)

multiExpr :: Parser [LispVal]
multiExpr = expr `endBy1` spaces <* eof

readExpr :: String -> Either ParseError [LispVal]
readExpr = parse multiExpr "scheme"
