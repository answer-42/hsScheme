module Main where

import AST
import InternalDefinitions
import TransformTopDefinitions
import ApplyMacros

mainTest input = (removeIntDef . applyMacros . transformTopDef) input
