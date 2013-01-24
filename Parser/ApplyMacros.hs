module Parser.ApplyMacros where

import Parser.AST

{- Datatype where we can match the keyword in our AST tree and then transform it
 - following the transformer rules. Each transformer rule has its own pattern
 - to match against.
 -
 - Remark: Every pattern of ONE rule gets his own Macro!!!
 -}
data Macro = Macro { pattern :: [LispVal]     -- the whole pattern, including literals
                    ,literals :: [LispVal]    -- List of Symbols
                    ,transformer :: [LispVal] -- What we get at the end
                   } deriving (Eq, Show)

{- Standard macros from the R5RS (Those that are right now in Parser.Transformation. 
   will be read from a seperate file. 
 -}


{- TODO:
 - We have to implement a macro reader that finds all syntax rules and adds them
 - to the list of all known macros. It also has to return the new AST without
 - the syntax rules!
 -}

-- readMacros :: AST -> Macro 
readMacros  = id  

removeMacros :: AST -> AST
removeMacros = id

applyMacros :: AST -> AST
applyMacros = id 

{- TODO
 - Apply macros to AST
 -}
