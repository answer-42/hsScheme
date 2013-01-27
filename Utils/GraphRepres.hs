module Utils.GraphRepres where

-- import Data.GraphViz
-- import Data.Graph.Inductive.Graph
-- import Data.Graph.Inductive.Tree

import Parser.AST
import Parser.Parser

import Data.List

indent :: Int -> String
indent l = l `listMul` "\t"
  where listMul l x = concat $ replicate l x

astToGraph :: AST -> [String]
astToGraph = map (addToGraph 0)
  where addToGraph l (List x)    = indent l ++  "LIST\n" ++ concatMap (addToGraph (l+1)) x   
        addToGraph l (Symbol s)  = indent l ++ "Symbol " ++ s ++ "\n"
        addToGraph l (Number i)  = indent l ++ "Number " ++ show i ++ "\n"
        addToGraph l (Bool b)    = indent l ++ "Bool " ++ show b ++ "\n"
        -- TODO Add Dotted List
        addToGraph l (Char c)    = indent l ++ "Char " ++ show c ++ "\n"
        addToGraph l _ = indent l ++ "...\n"

mkASTGraph :: String -> String
mkASTGraph input = case readExpr input of
                        Right ast -> concat $ astToGraph ast
                        Left err  -> show err

