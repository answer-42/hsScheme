module ICR.Types where

import qualified Data.List as L
import Parser.AST

interMap s f = L.intercalate s . map f

type ICR = [TopLevel]

type Name = String
type Var = (Name, Type)
type Args = [(Name, Type)]
type Cells = Int

data Type = IInt
     --   | IFloat
          | IChar
          | IArray Type Cells
          | IPointer Type

data TopLevel = FunDef Function
              | Stmt Statement

data Decl = VarDef Var (Maybe Expr)

data Statement = Return Expr
               | Expr Expr
               | If Expr Block (Maybe Block)
               | SBlock Block  

newtype Block = Block [Statement]

data Expr = Funcall Name [Expr]
          | Assign Name Expr
          | Decl Decl  
          | Arith ArithExpr
          | Const ConstExpr 
          | Conditional Expr Expr Expr  
          | Subscript Expr Int
            
data ConstExpr = CChar Char
               | CInt Int  
               | CString String  
               | Var Name  

data ArithExpr = Add Expr Expr
               | Sub Expr Expr
               | Mult Expr Expr
               | Div Expr Expr

data Function = Function {
    name :: String,
    returnType :: Type,
    arguments :: Args,
    body :: Block
  }

instance Show TopLevel where
  show (FunDef f) = show f
  show (Stmt s) = show s

instance Show Block where
  show (Block s) = "{\n  " ++ interMap ";\n  " show s ++ ";\n}"
  
instance Show Decl where
  show (VarDef (n, t) e) =
    (case t of
        IArray t' _ -> show t' ++ " " ++ n ++ "[]"
        t' -> show t ++ " " ++ n) ++ maybe "" ((" = " ++) .  show) e
  
instance Show ConstExpr where
  show (CChar c) = show c
  show (CInt n) = show n
  show (CString s) = show s
  show (Var n) = n

instance Show ArithExpr where
  show (Add a b) = show a ++ " + " ++ show b
  show (Sub a b) = show a ++ " - " ++ show b
  show (Mult a b) = show a ++ " * " ++ show b
  show (Div a b) = show a ++ " / " ++ show b

instance Show Expr where
  show (Funcall n a) = n ++ "(" ++ interMap ", " show a ++ ")"
  show (Assign n e) = n ++ " = " ++ show e
  show (Decl d) = show d
  show (Arith ae) = show ae
  show (Const ce) = show ce
  show (Conditional p a b) = "(" ++ show p ++ " ? " ++ show a ++ " : " ++ show b ++ ")"
  show (Subscript e i) = show e ++ "[" ++ show i ++ "]"

instance Show Statement where
  show (Return e) = "return " ++ show e
  show (Expr e) = show e
  show (SBlock b) = show b
  
instance Show Type where
  show IInt = "int"
  show IChar = "char"
  show (IPointer t) = show t ++ "*"
  show (IArray t _) = show t

instance Show Function where
  show (Function n rt a b) =
    show rt ++ " " ++ n ++ "(" ++ interMap ", " showArg a ++ ") " ++ show b
    where showArg (n, t) =
            case t of
              IArray t' i -> show t' ++ " " ++ n ++ "[]"
              t' -> show t' ++ " " ++ n
  
