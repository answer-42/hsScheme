module Parrot.Parrot where

import Parser.AST


astToParrot :: AST -> String
astToParrot = show
