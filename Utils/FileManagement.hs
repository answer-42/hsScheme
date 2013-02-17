module Utils.FileManagement where

doCompAct :: (FilePath -> String) -> FilePath -> IO String
doCompAct g fn = readFile fn >>= (return . g)

outputAct :: (FilePath -> String) -> FilePath -> IO ()
outputAct g fn = doCompAct g fn >>= putStrLn
