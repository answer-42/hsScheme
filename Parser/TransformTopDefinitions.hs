module Parser.TransformTopDefinitions where

import Parser.AST

{- TODO: Clean up the code 
 -}

{- REMARK: I assume that if you have a dotted list as first argument for define, that
 -         the dotted list is of the form (a . b), where a and b are symbols, a can also be
 _         empty.
 -}

transformTopDef :: [LispVal] -> [LispVal]
transformTopDef = map transform
  where transDefLam x xs =
          List [Symbol "define",
                head x,
                List $ [Symbol "lambda", List $ tail x] ++ map transform xs]   
        transDotDefLam xi xe xs =
          List $ [Symbol "define"] ++ xi ++ [List $ [Symbol "lambda", xe] ++ map transform xs]
        transform l@(List x) =
          case x of
            Symbol "define":List y:ys -> transDefLam y ys 
            Symbol "define":DottedList yi ye:ys -> transDotDefLam yi ye ys
            _ -> l
