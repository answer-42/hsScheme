-- file: ICR.hs

{-# LANGUAGE ExistentialQuantification #-}
module ICR.ICR where

type Var = String
type Name = String

data Type
   = Int
   | Float
   | Char
   | Array Type
   | Pointer Type

data TopLevelDecl
   = FunDef Function
   | Typedef Var Var
     
data Decl
   = VarDef Var (Maybe Expr)

data Statement
   = Return Expr
   | Expr Expr
     
data Expr
   = Funcall Name
   | Assign Var Expr
   | Arith ArithExpr

data ArithExpr
   = Undefined

data Function
   = Function {
     name :: String,
     returnType :: Type,
     arguments :: [(Name, Type)],
     body :: [Statement]
     }
     
