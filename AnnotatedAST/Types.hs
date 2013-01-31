module AnnotatedAST.Types where

import Parser.AST

data AType = 
  AInt
  | AString
  | AChar
  | ABool
  | AFunction [AType] AType
  | AUndefined

data AnnLispVal =
  ASymbol String AType
  | AList [AnnLispVal]
  | ADottedList [AnnLispVal] AnnLispVal
--  | Vector
  | ANumber Integer
--  | Float Double
  | AString String
  | ABool Bool
  | AChar Char
  | ANil
    deriving (Eq)

type AST2 = [AnnLispVal]
