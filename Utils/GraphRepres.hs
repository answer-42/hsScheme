module Utils.GraphRepres where

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import Parser.AST
import Parser.Parser

-- Given an AST, create a graph G, and then represent it in Graphviz

graph = mkGraph [(0, "TOP")] [] :: Gr String Int

-- astToGraph :: AST -> Gr String Int 
astToGraph ast = foldl (\g e -> let ns = labNodes g
                                    pre = foldr (\(e,_) a -> if e>a then e else a) 0 ns
                                in addNode g pre e 0)
                                --insEdge (0,pre+1, -1) (insNode (pre+1,show e) a))
                       graph ast
  where addNode g pre (List x) base = addListNode g pre base x
        -- TODO add support for dotted list
        addNode g pre x base      = insEdge (base, pre+1, -1) $ insNode (pre+1, show x) g

        addListNode g pre preN [] = addNode g pre (String "NIL" :: LispVal) preN
        addListNode g pre preN (x:xs) =
          let g' = addNode g pre x preN
              pre' = foldr (\(e,_) a -> if e>a then e else a) 0 $ labNodes g'
          in addListNode g'  pre' (pre') xs

mkASTGraph :: String -> String
mkASTGraph input = case readExpr input of
                        Right ast -> (show .astToGraph) ast
                        Left err  -> show err
