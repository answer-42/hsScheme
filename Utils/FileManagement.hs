module Utils.FileManagement where

import Control.Monad (liftM)

doCompAct :: (FilePath -> String) -> FilePath -> IO String
doCompAct g fn = liftM g (readFile fn)

outputAct :: (FilePath -> String) -> FilePath -> IO ()
outputAct g fn = doCompAct g fn >>= putStrLn
