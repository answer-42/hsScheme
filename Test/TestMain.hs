module Test.TestMain where

import qualified Test.InternalDefinitions as ID
import qualified Test.Macros as M
-- import qualified Test.Parser as P

test = sequence_ [ID.test, M.test] 