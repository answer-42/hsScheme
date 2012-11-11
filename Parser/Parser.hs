-- file: parser.hs

module Parser.Parser where

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import Control.Monad (join)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (char, string)
import qualified Text.ParserCombinators.Parsec as P

import Parser.AST

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
symbol = Symbol <$> ((:) <$> letter
                         <*> many (letter <|> specialChar <|> digit))
  where specialChar = oneOf "!$%&*+-./:<=>?@^_~"

bool :: Parser LispVal
bool = do
  p <- P.char '#' *> oneOf "tf"
  return (Bool (case p of
                   't' -> True
                   'f' -> False))

integer :: Parser LispVal
integer = do
  s <- sign
  x <- digits
  return $ Number $ s (read x)

float :: Parser LispVal
float = do
  s <- sign
  beforePoint <- digits
  P.char '.'
  afterPoint <- digits
  return $ Float $ s (read (beforePoint ++ "." ++ afterPoint))

dottedList :: Parser LispVal
dottedList = do
  firstPart <- spaces >> endBy expr spaces
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
   <|> try float
   <|> try integer
   <|> try quote
   <|> between lparen rparen (try dottedList <|> list)

multiExpr :: Parser [LispVal]
multiExpr = expr `endBy1` spaces <* eof

readExpr :: String -> Either ParseError [LispVal]
readExpr input = parse multiExpr "scheme" input

-- tests

testReadExpr :: String -> [String]
testReadExpr s = case readExpr s of
  Left e -> ["error: " ++ show e]
  Right a -> map show a
    -- case a of
    -- Char c -> "char: " ++ [c]
    -- String s -> "string: " ++ s
    -- Symbol s -> "symbol: " ++ s
    -- a@(Bool _) -> "boolean: " ++ show a
    -- Number n -> "number: " ++ show n
    -- Float r -> "float: " ++ show r
    -- a@(List _) -> "list: " ++ show a
    -- a@(DottedList _ _) -> "dottedlist: " ++ show a

test = mapM_ (print . testReadExpr) tests
  where tests = ["#\\a", "\"hello \n\\\"scheme\\\"\"",
                 "123", "1.42", "symbol42", "(a b c)",
                 "(h (54 2) . e)", "(a b.c)", "'('u 3)",
                 setLambda, show carLambda, "#t", "#f"]
        setLambda =
          show (createLambda ["a", "b", "c"]
                             [List [Symbol "set!", Symbol "a", Number 1], 
                              Symbol "a"])
