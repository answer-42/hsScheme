module Test.Main where

import qualified Test.ICR.Main as I
import qualified Test.Parser.Main as P

annotate t a =
  let t' = "Tests from " ++ t
      in putStrLn (t' ++ "\n" ++ replicate (length t') '-') >> a >> putStrLn ""

test = sequence_ [annotate "ICR" I.test, annotate "Parser" P.test]