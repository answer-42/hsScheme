module Parser.TransformTopDefinitions where

import Parser.AST

{- TODO: Clean up the code 
 -}

{- REMARK: I assume that if you have a dotted list as first argument for define, that
 -         the dotted list is of the form (a . b), where a and b are symbols.
 -}

transformTopDef :: [LispVal] -> [LispVal]
transformTopDef = map transform
  where transDefLam x xs =
            List [sDef, head x, List $ [sLam, List $ tail x] ++ map transform xs]   
        transDotDefLam xi xe xs =
            List [sDef, head xi, List $ [sLam, xe] ++ map transform xs]
        transform l@(List x) =
          case x of
            Symbol "define":List y:ys -> transDefLam y ys 
            Symbol "define":DottedList yi ye:ys -> transDotDefLam yi ye ys
            Symbol "let":x:xs -> List $ sLet:[x] ++ transformTopDef xs
            _ -> l
        transform r = r
