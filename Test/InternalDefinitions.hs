module Test.InternalDefinitions where

import Parser.AST
import Parser.Parser
-- import Parser.InternalDefinitions
import Parser.Transformations

tests = [  ("(define x (lambda (y) (define a b) (+ 11 2) a))",
           "[(define x (lambda (y) (letrec ((a b)) (+ 11 2) a)))]")
         ,("1", "[1]")
         ,("(let ((x y) (z w))  (define a b) a)",
           "[(let ((x y) (z w)) (letrec ((a b)) a))]")
        ]

testReadExpr :: (String, String) -> String
testReadExpr s = case readExpr $ fst s of
                   Right exp -> if (show . removeIntDef) exp == snd s
                                then show "Success" 
                                else (show . removeIntDef) exp
                   Left e -> show e 

test = mapM_  (putStrLn . testReadExpr) tests