module Parser.InternalDefinitions where

import Parser.AST

removeIntDef :: AST -> AST
removeIntDef = map transIntDef 
  where removeDef :: AST -> LispVal -> LispVal
        removeDef d (List (Symbol "lambda":x:xs)) =
            List $ d ++ [List $ [Symbol "lambda", x] ++ changeDef xs]
        removeDef d (List xs) = List $ d ++ changeDef xs

        changeDef :: AST -> AST
        changeDef xs = 
            [List $ [Symbol "letrec", 
                     List $ map (\y -> List [fst y, snd y]) defines] ++ rest]
            where defines = map (\y -> case y of
                                         List [Symbol "define",a,as] -> (a,removeDef [] as))
                            $ filter isDefine xs
                  rest = filter (not . isDefine) xs

        transIntDef :: LispVal -> LispVal
        transIntDef (List (Symbol "let":x:xs)) =
            removeDef [Symbol "let", x] (List xs)
        transIntDef (List [Symbol "define",x,xs]) = 
            removeDef [Symbol "define",x] xs
        transIntDef r = r
