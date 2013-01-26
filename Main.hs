module Main where

import System.Environment (getArgs)
import Control.Monad (unless, void)

import Parser.AST
import Parser.Parser
import Parser.Transformations
import Parser.ApplyMacros

import Test.TestMain

main = do
  args <- getArgs
  unless (null args)
         (case head args of
               "-t"      -> void doTests
               "-c"      -> putStrLn $ compile $ args !! 1
               -- Insert new commands here
               otherwise -> void doUsage)
  where doTests = test
        doUsage = putStrLn "TODO: Usage text"

compile input = case readExpr input of
                     Right ast -> let x      = (removeIntDef . transformTopDef) ast
                                      macros = readMacros x
                                  in show $ (letToLambda . 
                                      letStarTrans . 
                                      ifTrans . 
                                      applyMacros macros) 
                                      $ removeMacros x
                     Left err  -> show err
