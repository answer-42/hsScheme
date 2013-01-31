module AnnotatedAST.AddAnnotations where

import qualified Data.Map as M
import AnnotatedAST.Types

type Name = String

-- OpTypes argument type | return type
-- assumption: primops have homogeneous arg types
data PrimOpType = OpTypes AType AType

primitiveOps :: M.Map Name PrimOpType
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


