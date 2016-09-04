module Befunge93Interpreter where

import Data.Maybe
import Befunge93Data
import Befunge93Logic

interpret :: FilePath -> IO ()
interpret path = do
  file <- readFile path
  let (grid, gridSize) = fromJust $ fungify file
  let program = (grid, gridSize, (0,0), E, [], Normal)
  --mapM_ putStrLn grid
  --putStrLn $ show grid
  befungeloop program

befungeloop :: Program -> IO ()
befungeloop p@(grid, gridSize, pcPos, pcDir, stack, mode) = do
  let symb = getSymbol grid pcPos
  case mode of 
    Normal -> do
      case symb of
        '@' -> do
          putStr $ "\n"
          return ()
        '#' -> befungeloop $ step p 2
        _   -> do 
              p' <- evaluate p
              let p'' = step p' 1
              befungeloop p''
    String -> do
      p' <- evaluate p
      let p'' = step p' 1
      befungeloop p''
