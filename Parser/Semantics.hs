-- file: Semantics.hs

module Parser.Semantics where

import Parser.AST
    
checkDefines :: [LispVal] -> Bool
checkDefines = go check True
  where go _ p [] = p
        go _ False _ = False
        go f _ (a:as) = go f (f a) as
        
        check a =
          case a of
            List (Symbol "define":List xs:body) ->
              all isSymbol xs && checkDefines body
            List (Symbol "define":DottedList xs x:body) ->
              length xs <= 1 && all isSymbol xs && isSymbol x && checkDefines body
            List (Symbol "define":[Symbol _, List l]) -> checkDefines l
            _ -> True
        