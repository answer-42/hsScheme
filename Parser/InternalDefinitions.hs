module Parser.InternalDefinitions where

import Parser.AST

{- TODO
 - Transform internal definitions to letrec definitions.
 - See r5rs 5.2.2
 -}

removeIntDef exprs = map transIntDef exprs
  where removeDef x = List x 
        transIntDef l@(List x) 
            | (Symbol "define") `elem` x = removeDef x
            | otherwise                  = l

