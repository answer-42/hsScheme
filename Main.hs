module Main where

import System.Environment (getArgs)
import Control.Monad (unless, when, void)

import Parser.AST
import Parser.Parser
import Parser.Transformations
import Parser.ApplyMacros

import Parrot.Parrot

import Utils.GraphRepres
import Utils.FileManagement

import Test.Main

main = do
  args <- getArgs
  unless (null args) $ case head args of
    "-t" -> test
    "-p" -> do 
      when (null (tail args)) showUsage
      outputAct parrotC $ args !! 1
    "-c" -> do
      when (null (tail args)) showUsage 
      outputAct astC $ args !! 1
    "-g" -> do
      when (null (tail args)) showUsage
      outputAct graphC $ args !! 1
    -- Insert new commands here
    "--help" -> showUsage
    _ -> showUsage
  where showUsage = putStrLn $ unlines 
          ["", "Usage", "-----",
           "  -t:     run the test suite",
           "  -c <s>: compile the file <s> to AST",
           "  -p <s>: compile the file <s> to Parrot",
           "  -g <s>: shows the lisp expression file in a tree representation",
           "  --help: shows this usage message"]

astC input = case readExpr input of
                  Right ast -> show $ appAST ast
                  Left err  -> show err

parrotC input = case readExpr input of
                     Right ast -> astToParrot $ appAST ast
                     Left err  -> show err

graphC input = case readExpr input of
                    Right ast -> mkASTGraph $ appAST ast
                    Left err  -> show err

appAST ast = let x = removeIntDef $ transformTopDef ast
                 macros = readMacros x
             in  (letToLambda . 
                  letStarTrans . 
                  ifTrans . 
                  applyMacros macros) 
                  $ removeMacros x
                
