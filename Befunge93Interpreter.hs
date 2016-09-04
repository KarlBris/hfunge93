module Befunge93Interpreter where

import Data.Maybe
import Befunge93Data
import Befunge93Logic

interpret :: FilePath -> IO ()
interpret path = do
  file <- readFile path
  let grid = fungify file
  mapM_ (putStrLn) (fromJust grid)
  --putStrLn $ show grid