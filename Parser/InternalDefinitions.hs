module Parser.InternalDefinitions where

import Parser.AST

{- TODO
 - Transform internal definitions to letrec definitions.
 - See r5rs 5.2.2
 -}

removeIntDef exprs = map transIntDef exprs
  where transIntDef x = id x 

