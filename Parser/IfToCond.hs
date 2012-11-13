module Parser.IfToCond where

import Parser.AST

ifTrans :: AST -> AST
ifTrans = map changeIf
    where changeIf :: LispVal -> LispVal
          changeIf (List [Symbol "if",x,xs,xss]) =
              List [sCond, List $ ifTrans [x, xs], List [sElse, changeIf xss]]
          changeIf (List [Symbol "if",x,xs]) =
              List [sCond, List $ ifTrans [x, xs]]
          changeIf (List x) = List $ ifTrans x
          changeIf (DottedList x xs) = DottedList (ifTrans x) (changeIf xs)
          changeIf r = r   