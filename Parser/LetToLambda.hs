module Parser.LetToLambda where

import Parser.AST

letToLambda :: AST -> AST
letToLambda = map changeLet
    where changeLet :: LispVal -> LispVal
          -- changeLet (List [Symbol "let",x,xs]) =
          changeLet = id
              