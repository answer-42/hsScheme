-- file: ast.hs

module Ast where

import Control.Applicative ((<$>))
import Data.List (intercalate)

data LispVal =
  Symbol String
  | List [ LispVal ]
  | DottedList [ LispVal ] LispVal
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

lambda :: [String] -> [LispVal] -> LispVal
lambda args body = List ([Symbol "lambda", List (Symbol <$> args)] ++ body)

-- tests
carLambda :: LispVal
carLambda = lambda ["a", "b"] [List [Symbol "car", Symbol "a"], Symbol "a"]