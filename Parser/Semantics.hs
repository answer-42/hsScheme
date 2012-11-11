-- file: Semantics.hs

module Parser.Semantics where

import Parser.AST

-- data LispVal =
--   Symbol String
--   | List [LispVal]
--   | DottedList [LispVal] LispVal
--   | Number Integer
--   | Float Double
--   | String String
--   | Bool Bool
--   | Char Char
    
checkDefines :: [LispVal] -> Bool
checkDefines = foldl fn True
  where fn a e = undefined
        
        