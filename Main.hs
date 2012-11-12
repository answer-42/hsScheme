module Main where

import Parser.AST
import Parser.Parser
import Parser.InternalDefinitions
import Parser.TransformTopDefinitions
import Parser.ApplyMacros

mainTest input = case readExpr input of
                   Right ast -> show $ (applyMacros . removeIntDef . transformTopDef) ast
                   Left err  -> show err
