module Test.Macros where

import Parser.ApplyMacros
import Parser.Parser

tests = [("(change x) (define-syntax change (syntax-rules () ((change x) (set! x 10)) ((change x y) (set! x y))))"
         ,"(set! x 10)")
        -- ,(""
        --  ,[])
        ]

testMacros s = case readExpr $ fst s of
                 Right exp -> if (show . readMacros) exp == snd s
                              then show "Success"
                              else (show . readMacros) exp
                 Left e -> show e 

test = map testMacros tests