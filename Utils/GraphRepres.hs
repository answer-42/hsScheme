module Utils.GraphRepres where

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Parser.AST
import Parser.Parser

-- Given an AST, create a graph G, and then represent it in Graphviz

{- TODO
- * Change names
- * Proper testing
- * graphical representation
  -}

graph = mkGraph [(0, "TOP")] [] :: Gr String Int


-- Get the node with the highest node number, where Node Numbers are positive
-- integers.
getLastNode :: [(Node, a)] -> Node
getLastNode = foldr (\(e, _) a -> if e > a then e else a) 0

-- astToGraph :: AST -> Gr String Int 
astToGraph = foldl (\g e -> let pre = getLastNode $ labNodes g
                            in addNode g pre e 0)
                       graph 
  where addNode g pre (List x) base = addListNode g pre base x
        -- TODO add support for dotted list
        addNode g pre x base      = insEdge (base, pre+1, -1) $ insNode (pre+1, show x) g

        addListNode g pre preN [] = addNode g pre (String "NIL" :: LispVal) preN
        addListNode g pre preN (x:xs) =
          let g' = addNode g pre x preN
              pre' = getLastNode $ labNodes g'
          in addListNode g'  pre' pre' xs

mkASTGraph :: String -> String
mkASTGraph input = case readExpr input of
                        Right ast -> (show .astToGraph) ast
                        Left err  -> show err
