module Main where

import Parser.AST
import Parser.Parser
import Parser.InternalDefinitions
import Parser.TransformTopDefinitions
import Parser.ApplyMacros
import Parser.IfToCond
import Parser.LetStarToLet

mainTest input = case readExpr input of
                   Right ast -> show $ 
                                (letStarTrans .
                                 ifTrans . 
                                 applyMacros . 
                                 removeIntDef .
                                 transformTopDef
                                ) ast
                   Left err  -> show err
