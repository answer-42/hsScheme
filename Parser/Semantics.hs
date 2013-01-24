-- file: Semantics.hs

module Parser.Semantics where

import Parser.AST

-- | Returns True if the defines in the program are well formed
--   and False otherwise.
checkDefines :: [LispVal] -> Bool
checkDefines = go check True
  where go _ p [] = p
        go _ False _ = False
        go f p (a:as) = go f (f p a) as

        -- | Checks if the defines are well formed.
        check _ a =
          case a of
            List (Symbol "define":List xs:body) ->
              all isSymbol xs && checkDefines body
            List (Symbol "define":DottedList xs x:body) ->
              length xs == 1 && all isSymbol xs && isSymbol x
                             && checkDefines body && correctOrder body
            List (Symbol "define":[Symbol _, List l]) -> checkDefines l
            _ -> True

        -- | Checks if the defines only occur at the beginning of the body.
        correctOrder :: [LispVal] -> Bool
        correctOrder = all (not . isDefine) . dropWhile isDefine

-- | Returns True if the syntax-rules has the correct form.
checkSyntaxRules :: String -> LispVal -> Bool
checkSyntaxRules name (List (Symbol "syntax-rules":List lits:xs)) =
  all isSymbol lits && checkSyntaxRulesBody xs
  where checkSyntaxRulesBody = all isTwoLists
        isTwoLists (List [List l, List _]) = all isSymbol l
        isTwoLists _ = False
