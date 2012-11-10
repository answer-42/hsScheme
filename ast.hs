-- file: ast.hs

module Ast where

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

-- We could also do something like:
-- | Lambda { lArg :: [ String ]
--           ,lbody :: [ LispVal ]
--          }
