module Parser.InternalDefinitions where

import Parser.AST
import Data.List (foldr)

{- TODO
 - Transform internal definitions to letrec definitions.
 - See r5rs 5.2.2
 -}

removeIntDef :: [LispVal] -> [LispVal]
removeIntDef = map transIntDef 
  where removeDef :: [LispVal] -> [LispVal] -> LispVal
        removeDef d xs = List $ (d++xs) --List $ foldr (\a b -> ) 
        
        transIntDef :: LispVal -> LispVal
        transIntDef (List (Symbol "define":x:xs:xss)) = removeDef (Symbol "define":x:xs:[]) xss
        transIntDef r = r

-- removeIntDef = id
