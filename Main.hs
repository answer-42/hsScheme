module Main where

import System.Environment (getArgs)
import Control.Monad (unless, when, void)

import Parser.AST
import Parser.Parser
import Parser.Transformations
import Parser.ApplyMacros

import Utils.GraphRepres

import Test.Main

main = do
  args <- getArgs
  unless (null args) $ case head args of
    "-t" -> test
    "-c" -> do
      when (null (tail args)) showUsage
      putStrLn $ compile $ args !! 1
    "-g" -> do
      when (null (tail args)) showUsage
      putStrLn $ mkASTGraph $ args !! 1 
    -- Insert new commands here
    "--help" -> showUsage
    _ -> showUsage
  where showUsage = putStrLn $ unlines 
          ["", "Usage", "-----",
           "  -t:     run the test suite",
           "  -c <s>: compile the string <s>",
           "  -g <s>: shows the lisp expression <s> in a tree representation",
           "  --help: shows this usage message"]

compile input = case readExpr input of
                     Right ast -> let x      = removeIntDef $ transformTopDef ast
                                      macros = readMacros x
                                  in show $ (letToLambda . 
                                      letStarTrans . 
                                      ifTrans . 
                                      applyMacros macros) 
                                      $ removeMacros x
                     Left err  -> show err
