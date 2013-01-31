module ICR.ASTToIRC where

import ICR.Types
import Parser.AST

--| Precondition: Macros are already applied
astToICR :: AST -> ICR
astToICR = map f
  where f :: LispVal -> TopLevel
        f x = case x of
                List [Symbol "define",Symbol n,body] -> transToTop n body
                otherwise -> undefined

transToTop :: String -> LispVal -> TopLevel
transToTop n body = case body of
                      List (Symbol "lambda":List args:body) -> 
                        FunDef $ Function{ name = n
                                          ,arguments = args}  
