-- file: AndOrToIf.hs

module Test.AndOrToIf where

import Parser.AndOrToIf

andOrTests = ["(and)",
              "(and a)",
              "(and a b c)",
              "(or)",
              "(or a)",
              "(or a b c)",
              "(and (or a b) b (and (or) a))",
              "(or #t (and) (or h (and (and a) b)) c)"]