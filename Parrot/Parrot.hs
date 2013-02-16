module Parrot.Parrot where

import Parser.AST


astToParrot :: AST -> String
astToParrot ast = ".sub main :main\n"++ (foldr f "" topLevel) ++ ".end"
  where f (List [Symbol "display",Symbol x]) a = a++"say "++x++"\n"
        f (List [Symbol "display",String x]) a = a++"say \""++x++"\"\n"
        f _ _ = error "Something went wrong -- astToParrot"
        
        topLevel = filter (\x -> case x of
                                     List (Symbol "define":_)  -> False
                                     otherwise                 -> True) ast

