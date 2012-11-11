-- file: AST.hs

module AST where

import Control.Applicative ((<$>))
import Data.List (intercalate)

data LispVal =
  Symbol String
  | List [LispVal]
  | DottedList [LispVal] LispVal
--  | Vector
  | Number Integer
  | Float Double
  | String String
  | Bool Bool
  | Char Char
    deriving (Eq)

instance Show LispVal where
  show (Symbol s) = s
  show (List l) = "(" ++ intercalate " " (show <$> l) ++ ")"
  show (DottedList l v) = "(" ++ intercalate " " (show <$> l) ++ " . " ++ show v ++ ")"
  show (Number n) = show n
  show (Float r) = show r
  show (String s) = s
  show (Bool p) = if p then "#t" else "#f"
  show (Char c) = [c]

createLambda :: [String] -> [LispVal] -> LispVal
createLambda args body = List ([Symbol "lambda", List (Symbol <$> args)] ++ body)

createQuote :: LispVal -> LispVal
createQuote v = List [Symbol "quote", v]

-- tests
carLambda :: LispVal
carLambda = createLambda ["a", "b"] [List [Symbol "car", Symbol "a"], Symbol "a"]
