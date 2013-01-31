module AnnotatedAST.Types where

import Parser.AST

data AType = 
  AInt
  | AString
  | AChar
  | AFunction AType

data AnnLispVal =
  Symbol String AType
  | List [AnnLispVal]
  | DottedList [AnnLispVal] AnnLispVal
--  | Vector
  | Number Integer
--  | Float Double
  | String String
  | Bool Bool
  | Char Char
  | Nil
    deriving (Eq)


