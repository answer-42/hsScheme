module Main where

import Parser.AST
import Parser.Parser
import Parser.Transformations
import Parser.ApplyMacros


mainTest input = case readExpr input of
                   Right ast -> show $ 
                                (letToLambda .
                                 letStarTrans .
                                 ifTrans . 
                                 applyMacros . 
                                 removeIntDef .
                                 transformTopDef
                                ) ast
                   Left err  -> show err
