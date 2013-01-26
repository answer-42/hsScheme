module Utils.GraphRepres where

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Parser.AST
import Parser.Parser

-- Given an AST, create a graph G, and then represent it in Graphviz

{- TODO
- * Proper testing
- * graphical representation
- * Add in astToGraph proper support for List, so that a List gets a top Node.
  -}

graph = mkGraph [(0, "TOP")] [] :: Gr String ()

-- Get the node with the highest node number, where Node Numbers are positive
-- integers.
getLastNode :: [(Node, a)] -> Node
getLastNode = foldr (\(e, _) a -> if e > a then e else a) 0

-- Insert a Node with an edge to the source node (singular)
insNodeEdge :: Node -> Node -> a -> b -> Gr a b -> Gr a b
insNodeEdge s t n w g = insEdge (s, t, w) $ insNode (t, n) g 

astToGraph :: AST -> Gr String () 
astToGraph = foldl (\g e -> let l = getLastNode $ labNodes g
                            in addNode g l e 0)
                       graph 
  where addNode g l (List x) s = addListNode g l s x
        -- TODO add support for dotted list
        addNode g l x s        = insNodeEdge s (l+1) (show x) () g 

        addListNode g l s [] = addNode g l (String "NIL" :: LispVal) s
        addListNode g l s (x:xs) =
          let g'  = addNode g l x s
              l'  = getLastNode $ labNodes g'
          in addListNode g' l' l' xs

mkASTGraph :: String -> String
mkASTGraph input = case readExpr input of
                        Right ast -> (show .astToGraph) ast
                        Left err  -> show err
