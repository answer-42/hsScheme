module Test.Macros where

import Parser.ApplyMacros
import Parser.Parser

tests = [("(change x) (define-syntax () ((change x) (set! x 10)))"
         ,"(set! x 10)")
        -- ,(""
        --  ,[])
        ]

testMacros s = case readExpr $ fst s of
                 Right exp -> if (show . applyMacros) exp == snd s
                              then show "Success"
                              else (show . applyMacros) exp
                 Left e -> show e 

test = map testMacros tests