module AnnotatedAST.AddAnnotations where

import Control.Monad.State
import qualified Data.Map as M
import AnnotatedAST.Types

type Name = String

-- OpTypes argument type | return type
-- assumption: primops have homogeneous arg types
data OpType = OpTypes AType AType

type OpTypeMap = M.Map Name OpType

primitiveOps :: OpTypeMap
primitiveOps = M.fromList
  [("+", OpTypes AInt AInt)
  ,("-", OpTypes AInt AInt)
  ,("*", OpTypes AInt AInt)
  ,("/", OpTypes AInt AInt)
  ,("max", OpTypes AInt AInt)
  ,("min", OpTypes AInt AInt)
  ,("<", OpTypes AInt ABool)
  ,(">", OpTypes AInt ABool)
  ,("<=", OpTypes AInt ABool)
  ,(">=", OpTypes AInt ABool)
  ,("integer?", OpTypes AUndefined ABool)
  ,("string?", OpTypes AUndefined ABool)
  ,("zero?", OpTypes AUndefined ABool)
  ]

{-
annotate :: AST -> AST2
annotate x = evalState (mapM g x) primitiveOps
  where g x = case x of
                List [Symbol "define",Symbol n,body] 
                  -> AList (ASymbol "define" AUndefined:annotDefArg n body)
                otherwise -> undefined

annotDefArg :: Name -> LispVal -> [AnnLispVal]
annotDefArg n b = case b of
                    List [Symbol "lambda",List args] -> 
                      [ASymbol n (AFunction (map (const AUndefined) args) AUndefined)]
                    List (Symbol "lambda":List args:body) -> do
                      map <- get
                      let lastFn = case last body of
                            List (Symbol x:_) -> x
                            
                      case M.lookup 
                      [ASymbol n (AFunction [] ...), annotate body]
-}
