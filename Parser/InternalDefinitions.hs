module Parser.InternalDefinitions where

import Parser.AST
-- import Data.List (foldr)
-- import Control.Applicative (<$>)

{- TODO
 - Transform internal definitions to letrec definitions.
 - See r5rs 5.2.2, also for toplevel let expressions.
 -}

removeIntDef :: [LispVal] -> [LispVal]
removeIntDef = map transIntDef 
  where removeDef :: [LispVal] -> LispVal -> LispVal
        removeDef d (List (Symbol "lambda":x:xs)) =
            List $ d ++ [List $ [Symbol "lambda", x] ++ changeDef xs]

        changeDef xs = 
            [List $ [Symbol "letrec", 
                     List $ map (\y -> List [fst y, snd y]) defines] ++ rest]
            where defines = map (\y -> case y of 
                                         List (Symbol "define":a:as:[]) -> (a,as)) $
                            filter isDefine xs
                  rest = filter (not . isDefine) xs

        transIntDef :: LispVal -> LispVal
        transIntDef (List (Symbol "let":x:xs:[])) =
            removeDef [Symbol "let",x] xs
        transIntDef (List (Symbol "define":x:xs:[])) = 
            removeDef [Symbol "define",x] xs
        transIntDef r = r
