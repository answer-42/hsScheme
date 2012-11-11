module Main where

import AST
import InternalDefinitions
import ApplyMacros

mainTest input = (removeIntDef . applyMacros) input
