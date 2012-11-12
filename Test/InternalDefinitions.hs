module Test.InteralDefinitions where

import Parser.AST
import Parser.Parser
import Parser.InternalDefinitions

tests = [  ("(define x (lambda (y) (define a b) (+ 11 2) a))",
           "[(define x (lambda (y) (letrec ((a b)) (+ 11 2) a)))]")
         ,("1", "[1]")
         ,("(let ((x y)) (define a b) a)",
           "[(let ((x y)) (letrec ((a b)) a))]")
        ]

testReadExpr :: (String, String) -> String
testReadExpr s = case readExpr $ fst s of
                   Right exp -> if ((show . removeIntDef) exp == snd s) 
                                then show "Success" 
                                else (show . removeIntDef) exp
                   Left e -> show e 

test = map testReadExpr tests