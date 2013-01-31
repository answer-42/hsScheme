module Test.ICR.Types where

import ICR.Types

testFunction = Function {
    name = "fib",
    returnType = IInt,
    arguments = [("n", IInt)],
    body = Block [Expr $ Assign "a" (Const (CChar 'e')),
                  Expr $ Arith (Add (Const (CInt 1)) (Const (CInt 2))),
                  Return (Const (CInt 5))]
  }
               
test = mapM_ print [testFunction]