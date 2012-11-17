-- file: AST.hs
-- P.S. AST stands for abstract syntax tree ;-)

module Parser.AST where

import Control.Applicative ((<$>))
import Data.List (intercalate)

data LispVal =
  Symbol String
  | List [LispVal]
  | DottedList [LispVal] LispVal
--  | Vector
  | Number Integer
--  | Float Double
  | String String
  | Bool Bool
  | Char Char
  | Nil
    deriving (Eq)

type AST = [LispVal]

instance Show LispVal where
  show (Symbol s) = s
  show (List l) = "(" ++ sepBySpaces l ++ ")"
  show (DottedList l v) = "(" ++ sepBySpaces l ++ " . " ++ show v ++ ")"
  show (Number n) = show n
--  show (Float r) = show r
  show (String s) = "\"" ++ s ++ "\""
  show (Bool p) = if p then "#t" else "#f"
  show (Char c) = "#\\" ++ [c]
 
sepBySpaces :: [LispVal] -> String
sepBySpaces = intercalate " " . (show <$>)

createLambda :: [String] -> [LispVal] -> LispVal
createLambda args body = List ([Symbol "lambda", List (Symbol <$> args)] ++ body)

createQuote :: LispVal -> LispVal
createQuote v = List [Symbol "quote", v]

-- predicates
isSymbol :: LispVal -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isDefine :: LispVal -> Bool
isDefine (List (Symbol "define":_)) = True
isDefine _ = False

isIf :: LispVal -> Bool
isIf (List (Symbol "if":_)) = True
isIf _ = False

-- Symbol shortcuts
sIf = Symbol "if"
sDef = Symbol "define"
sLam = Symbol "lambda"
sLet = Symbol "let"
sLetr = Symbol "letrec"
sCond = Symbol "cond"
sElse = Symbol "else"
sT = Symbol "#t"
sF = Symbol "#f"