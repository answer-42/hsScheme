module Main where

import AST
import Parser
import InternalDefinitions
import TransformTopDefinitions
import ApplyMacros

mainTest input = case readExpr input of
                  Right ast -> show $ (removeIntDef . applyMacros . transformTopDef) ast
                  Left err  -> show err
