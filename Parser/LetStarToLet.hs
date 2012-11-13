module Parser.LetStarToLet where

import Parser.AST

letStarTrans :: AST -> AST
letStarTrans = map changeLetStar
    where changeLetStar :: LispVal -> LispVal
          changeLetStar (List [Symbol "let*",List x,xs]) =
              foldr (\e a -> List [sLet, List [changeLetStar e], a]) (emptyLet xs) x
          changeLetStar (List l) = List (letStarTrans l)
          changeLetStar (DottedList l e) = DottedList (letStarTrans l) (changeLetStar e)
          changeLetStar x = x

          emptyLet xs = List [sLet, List [], changeLetStar xs]