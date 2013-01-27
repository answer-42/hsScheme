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

type AGraph = Gr String ()

graph = mkGraph [(0, "TOP")] [] :: AGraph

-- Get the node with the highest node number, where Node Numbers are positive
-- integers.
getLastNode :: AGraph -> Node
getLastNode g = foldr (\(e, _) a -> if e > a then e else a) 0 $ labNodes g

-- Insert a Node with an edge to the source node (singular)
insNodeEdge :: Node -> Node -> a -> b -> Gr a b -> Gr a b
insNodeEdge s t n w g = insEdge (s, t, w) $ insNode (t, n) g 

astToGraph :: AST -> AGraph
astToGraph = foldl (addNode 0) graph
  where addNode s g (List x) = let l = 1 + getLastNode g 
                               in addListNode (insNodeEdge s l "LIST" () g) l l x
        -- TODO add support for dotted list
        addNode s g x        = let l = 1 + getLastNode g
                               in insNodeEdge s l (show x) () g 

        addListNode g l s []     = addNode s g (String "NIL" :: LispVal)
        addListNode g l s (x:xs) =
          let g'  = addNode s g x
              l'  = getLastNode g'
          in addListNode g' l' l' xs

mkASTGraph :: String -> String
mkASTGraph input = case readExpr input of
                        Right ast -> (show .astToGraph) ast
                        Left err  -> show err
