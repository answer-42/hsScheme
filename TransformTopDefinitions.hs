module TransformTopDefinitions where

import AST

{- TODO add some more cases -}

transformTopDef :: [LispVal] -> [LispVal]
transformTopDef = map transform
  where transDefLam x xs = List [Symbol "define", head x, List $ [Symbol "lambda", List $ tail x] ++ xs]   
        transform l@(List x) = case x of
                                (Symbol "define"):(List y):ys -> transDefLam y ys 
                                _ -> l
