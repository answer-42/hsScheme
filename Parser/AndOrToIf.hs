-- file: AndOrToIf.hs

module Parser.AndOrToIf where

import Parser.AST

-- | Transforms ands and ors to ifs.
-- Examples: (and a b c) ~> (if a (if b c #f) #f)
--           (or a b c) ~> (if a a (if b b (if c c #f)))
-- see Test.AndOrIf for the tests
andOrTrans :: AST -> AST
andOrTrans = map changeAndOr
  where changeAndOr :: LispVal -> LispVal
        changeAndOr (List [Symbol "and"]) = sT
        changeAndOr (List [Symbol "and", x]) = x
        changeAndOr (List (Symbol "and":xs)) =
          let ifxs = andOrTrans xs
          in foldr (\e a -> List [sIf, e, a, sF])
                   (last ifxs) (init ifxs)
        changeAndOr (List [Symbol "or"]) = sF
        changeAndOr (List [Symbol "or", x]) = x
        changeAndOr (List (Symbol "or":xs)) =
          foldl (\a e -> List [sIf, e, e, a]) sF (andOrTrans xs)
        changeAndOr x = x
        