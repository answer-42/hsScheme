module Parser.Transformations where

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

letStarTrans :: AST -> AST
letStarTrans = map changeLetStar
    where changeLetStar :: LispVal -> LispVal
          changeLetStar (List [Symbol "let*",List x,xs]) =
              foldr (\e a -> List [sLet, List [changeLetStar e], a])
                    (emptyLet xs) x
          changeLetStar (List l) = List (letStarTrans l)
          changeLetStar (DottedList l e) =
            DottedList (letStarTrans l) (changeLetStar e)
          changeLetStar x = x

          emptyLet xs = List [sLet, List [], changeLetStar xs]

letToLambda :: AST -> AST
letToLambda = map changeLet
    where changeLet :: LispVal -> LispVal
          changeLet (List [Symbol "let",List x,xs]) = 
            let xs' = case xs of 
                          List y -> y
                          y -> [y]
            in List $ (List $ [sLam,List $ map getSym x] ++
                       letToLambda xs'):map getArg x
          changeLet (List x) = List $ letToLambda x
          changeLet r = r

          getSym :: LispVal -> LispVal
          getSym (List x) = changeLet $ head x

          getArg :: LispVal -> LispVal
          getArg (List x) = changeLet $ (head . tail) x


-- | REMARK: I assume that if you have a dotted list as first
-- argument for define, that the dotted list is of the form (a . b),
-- where a and b are symbols.
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
            Symbol "let":x:xs -> List $ sLet:x:transformTopDef xs
            _ -> l
        transform r = r

removeIntDef :: AST -> AST
removeIntDef = map transIntDef 
  where removeDef :: AST -> LispVal -> LispVal
        removeDef d (List (Symbol "lambda":x:xs)) =
            List $ d ++ [List $ [sLam, x] ++ changeDef xs]
        removeDef d (List xs) = List $ d ++ changeDef xs

        changeDef :: AST -> AST
        changeDef xs = 
            [List $ if null defines 
                    then rest
                    else [sLetr,
                          List $ map (\y -> List [fst y, snd y]) defines]
                         ++ rest]
            where defines =
                    map (\y -> case y of
                            List [sDef,a,as] -> (a,removeDef [] as))
                    $ filter isDefine xs
                  rest = filter (not . isDefine) xs

        transIntDef :: LispVal -> LispVal
        transIntDef (List (Symbol "let":x:xs)) =
            removeDef [sLet, x] (List xs)
        transIntDef (List [Symbol "define",x,xs]) = 
            removeDef [sDef,x] xs
        transIntDef r = r