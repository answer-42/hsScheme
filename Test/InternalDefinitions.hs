module Test.InteralDefinitions where

import Parser.AST
import Parser.Parser
import Parser.InternalDefinitions

tests = [  ("(define x (lambda (y) (define a b) a))",
           "[(define x (lambda (y) (letrec ((a b)) a)))]")
         ,("1", "[1]")
        ]

testReadExpr :: (String, String) -> String
testReadExpr s = case readExpr $ fst s of
                   Right exp -> if ((show . removeIntDef) exp == snd s) 
                                then show "Success" 
                                else (show . removeIntDef) exp
                   Left e -> show e 

test = map testReadExpr tests