module Parser.ApplyMacros where

import Parser.AST

{- Datatype where we can match the keyword in our AST tree and then transform it
 - following the transformer rules. Each transformer rule has its own pattern
 - to match against. 
 -}
data Macro = Macro { keyword :: LispVal
                    ,transformers :: [LispVal] 
                   } deriving (Show)

{- Standard macros from the R5RS. 
 -}
macros = [
  
]

{- TODO:
 - We have to implement a macro reader that finds all syntax rules and adds them
 - to the list of all known macros. It also has to return the new AST without
 - the syntax rules!
 -}

applyMacros = id 

{- TODO
 - Apply macros to AST
 -}
