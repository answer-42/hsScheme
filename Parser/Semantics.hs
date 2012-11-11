-- file: Semantics.hs

module Semantics where

import Structure.AST

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
        
        