module Parser.ApplyMacros where

import Parser.AST
import qualified Data.List as L
import qualified Data.Map as M
import Data.Function
import Data.Maybe 
import Data.Char

{- Datatype where we can match the keyword in our AST tree and then transform it
 - following the transformer rules. Each transformer rule has its own pattern
 - to match against.
 -
 - Remark: Every pattern of ONE rule gets his own Macro!!!
 -}
data Macro = Macro {name :: String
                   ,arity :: Int -- the amount of pattern arguments         
                   ,pattern :: AST     -- the whole pattern, including literals and name
                   ,literals :: AST    -- List of Symbols
                   ,transformer :: AST -- What we get at the end
                   } deriving (Eq, Show)


-- List [Symbol "define-syntax", Symbol naam,
--      List [Symbol "syntax-rules", ...]]

readMacros :: AST -> [Macro] 
readMacros = concatMap readMacro . filter isMacro
--    filter isMacro
    where readMacro :: LispVal -> [Macro]
          readMacro (List [Symbol "define-syntax",Symbol x,List xs]) = createMacro x xs
          readMacro _ = []
          
          createMacro :: String -> AST -> [Macro]
          createMacro x (Symbol "syntax-rules":List y:z) =
              map (\(List [List pat,List trans]) -> Macro x (pred (length pat)) pat y trans) z
          createMacro _ _ = []

removeMacros :: AST -> AST
removeMacros = filter (not . isMacro)

-- Apply the function applyMacros to macro transformers
applyMacroToMacro = id

-- Preconditions: x should contain no define-syntax's
applyMacros :: [Macro] -> AST -> AST
applyMacros macros =
    let macroM = foldr (\e a -> M.insert (name $ head e) e a) M.empty 
                   $ L.groupBy ((==) `on` name) macros

        -- It is possible that there is NO macro with the right arity.
        getMacro n a = L.find ((a ==) . arity) $ fromJust $ M.lookup n macroM

        transform x m = 
            let varSub = zip (tail $ pattern m) $ applyMacros macros x
            in
              -- fromJust does never throw an error. 
              List $ map (\y -> fromMaybe y $ y `lookup` varSub) $ transformer m
                  
        applyMacro x@(List (Symbol s:xs)) = 
            if s `M.member` macroM 
            then case getMacro s (length xs) of
                   Just m  -> transform xs m
                   Nothing -> error $ "Arity mismatch in -- " ++ map toUpper s
            else x
        applyMacro x = x
 
    in map applyMacro

