module Parser.LetToLambda where

import Parser.AST

letToLambda :: AST -> AST
letToLambda = map changeLet
    where changeLet :: LispVal -> LispVal
          -- Normal Let
          changeLet (List [Symbol "let",List x,xs]) = 
              List $ [List [sLam, List $ map getSym x, xs]] ++ (map getArg x)

          -- TODO Named Let, recursive
          changeLet r = r

          getSym :: LispVal -> LispVal
          getSym (List x) = head x

          getArg :: LispVal -> LispVal
          getArg (List x) = (head . tail) x

              