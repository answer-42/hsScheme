module Test.Parser.Main where

import qualified Test.Parser.InternalDefinitions as ID
import qualified Test.Parser.Macros as M
-- import qualified Test.Parser as P

test = sequence_ [ID.test, M.test] 
