module Parrot.Parrot where

import Parser.AST


-- TODO
-- First we create some lists 
--  * List containing variable definitions (vars containing strings, numbers,
--    lists) that are global -> add to main
--  * List containing all global function definitions -> create own subroutines
--  for each
-- * The rest -> add to main

astToParrot :: AST -> String
astToParrot ast = ".sub main :main\n"++ (foldr f "" topLevel) ++ ".end\n\n" 
--                       ++ foldr g "" topDef
  where f (List [Symbol "display",Symbol x]) a = a++"say "++x++"\n"
        f (List [Symbol "display",String x]) a = a++"say \""++x++"\"\n"
        f _ _ = error "Something went wrong -- astToParrot"
        
--        g (List [Symbol "define",Symbol x, String s]) a = a++".local string '" ++ x ++ "'\n"
  --                                                        ++ x ++ " = \"" ++ s ++"\"\n" 
--        g _ a = a                                                                              

        isTopLevel = \x -> case x of
                                List (Symbol "define":_)  -> False
                                _                         -> True
        topLevel = filter isTopLevel ast
        topDef   = filter (not . isTopLevel) ast
