module Test.Parser.Parser where

import Parser.AST
import Parser.Parser

-- import Test.AndOrToIf
-- import Parser.AndOrToIf

tests = ["#\\a",
         "\"hello \n\\\"scheme\\\"\"",
         "123",
--         "1.42",
         "symbol42",
         "(a b c)",
         "(h (54 2) . e)",
         "(a b.c)",
         "'('u 3)",
         "#t",
         "#f",
         "(define a (lambda (b c) \"hello\"))\n(1 a 5)"]
                
testReadExpr :: String -> [String]
testReadExpr s = case readExpr s of
  Left e -> ["error: " ++ show e]
  Right a -> map show a

test = mapM_ (putStrLn . unwords . testReadExpr) tests
