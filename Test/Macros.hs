module Test.Macros where

import Parser.ApplyMacros
import Parser.Parser

tests = [("(change x) (define-syntax change (syntax-rules () ((change x) (set! x 10)) ((change x y) (set! x y))))"
         ,"(set! x 10)")
        ,("(change y z) (define-syntax change (syntax-rules () ((change x) (set! x 10)) ((change x y) (set! x y))))"
         ,"(set! y z)")
        ,("(change x) (define-syntax change (syntax-rules () ((change y) (set! y 10)) ((change x y) (set! x y))))"
         ,"(set! x 10)") 
        -- ,(""
        --  ,[])
        ]

runMacros x = let macros = readMacros x 
              in applyMacros macros $ removeMacros x  

testMacros s = case readExpr $ fst s of
                 Right exp -> if (show . runMacros) exp == snd s
                              then show "Success"
                              else (show . runMacros) exp
                 Left e -> show e 

test = map testMacros tests