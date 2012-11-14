module Parser.LetToLambda where

import Parser.AST

letToLambda :: AST -> AST
letToLambda = map changeLet
    where changeLet :: LispVal -> LispVal
          changeLet (List [Symbol "let",List x,xs]) = 
              let xs' = (case xs of 
                          (List y) -> y
                          y -> [y])
              in 
              List $ [List $ [sLam,List $ map getSym x] ++ (letToLambda xs')] ++ (map getArg x)
          changeLet (List x) = List $ letToLambda x
          changeLet r = r

          getSym :: LispVal -> LispVal
          getSym (List x) = changeLet $ head x

          getArg :: LispVal -> LispVal
          getArg (List x) = changeLet $ (head . tail) x

              