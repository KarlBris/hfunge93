module Befunge93Logic where

import Befunge93Data
import Data.Char (chr, ord)
import System.Random

step :: Program -> Int -> Program
step (grid, gridSize, pcPos, pcDir, stack, mode) i = (grid, gridSize, step' pcPos pcDir gridSize i, pcDir, stack, mode)

-- Wrapping step function
step' :: PCPos -> PCDir -> GridSize -> Int -> PCPos
step' (pcX, pcY) dir (xSize, ySize) length =
  case dir of
    N -> (pcX, (pcY - length) `mod` ySize)
    S -> (pcX, (pcY + length) `mod` ySize)
    E -> ((pcX + length) `mod` xSize, pcY)
    W -> ((pcX - length) `mod` xSize, pcY)

getSymbol :: Grid -> PCPos -> Char
getSymbol grid (x, y) = (grid !! y) !! x

fungify :: String -> Maybe (Grid, GridSize)
fungify file =
  if (width > 80 || height > 25)
    then Nothing
    else Just (file'', (width, height))
  where
    file' = lines file
    width = findLongestLine file'
    file'' = padLines file' width
    height = length file'

findLongestLine :: [String] -> Int
findLongestLine lines = maximum (map (length) lines)

padLines :: [String] -> Int -> [String]
padLines lines size = map (padLine size) lines

padLine :: Int -> String -> String
padLine size line = line ++ (replicate (size - (length line)) ' ')

evaluate :: Program -> IO Program
evaluate p@(grid, gridSize, pcPos, pcDir, stack, Normal) = do
  let symb = getSymbol grid pcPos
  case symb of
    ' ' -> return p
    '0' -> return (grid, gridSize, pcPos, pcDir, push stack 0, Normal)
    '1' -> return (grid, gridSize, pcPos, pcDir, push stack 1, Normal)
    '2' -> return (grid, gridSize, pcPos, pcDir, push stack 2, Normal)
    '3' -> return (grid, gridSize, pcPos, pcDir, push stack 3, Normal)
    '4' -> return (grid, gridSize, pcPos, pcDir, push stack 4, Normal)
    '5' -> return (grid, gridSize, pcPos, pcDir, push stack 5, Normal)
    '6' -> return (grid, gridSize, pcPos, pcDir, push stack 6, Normal)
    '7' -> return (grid, gridSize, pcPos, pcDir, push stack 7, Normal)
    '8' -> return (grid, gridSize, pcPos, pcDir, push stack 8, Normal)
    '9' -> return (grid, gridSize, pcPos, pcDir, push stack 9, Normal)
    '+' -> do
      let (stack', a) = pop stack
      let (stack'', b) = pop stack'
      return (grid, gridSize, pcPos, pcDir, push stack'' (a + b), Normal)
    '-' -> do
      let (stack', a) = pop stack
      let (stack'', b) = pop stack'
      return (grid, gridSize, pcPos, pcDir, push stack'' (b - a), Normal)
    '*' -> do
      let (stack', a) = pop stack
      let (stack'', b) = pop stack'
      return (grid, gridSize, pcPos, pcDir, push stack'' (a * b), Normal)
    '/' -> do
      let (stack', a) = pop stack
      let (stack'', b) = pop stack'
      return (grid, gridSize, pcPos, pcDir, push stack'' (b `div` a), Normal)
    '%' -> do
      let (stack', a) = pop stack
      let (stack'', b) = pop stack'
      return (grid, gridSize, pcPos, pcDir, push stack'' (b `mod` a), Normal)
    '!' -> do
      let (stack', a) = pop stack
      if (a == 0)
        then return (grid, gridSize, pcPos, pcDir, push stack' 1, Normal)
        else return (grid, gridSize, pcPos, pcDir, push stack' 0, Normal)
    '`' -> do
      let (stack', a) = pop stack
      let (stack'', b) = pop stack'
      if (b > a)
        then return (grid, gridSize, pcPos, pcDir, push stack'' 1, Normal)
        else return (grid, gridSize, pcPos, pcDir, push stack'' 0, Normal)
    '>' -> return (grid, gridSize, pcPos, E, stack, Normal)
    '<' -> return (grid, gridSize, pcPos, W, stack, Normal)
    '^' -> return (grid, gridSize, pcPos, N, stack, Normal)
    'v' -> return (grid, gridSize, pcPos, S, stack, Normal)
    '?' -> do
      i <- randomRIO (1, 4) :: IO Int
      case i of
        1 -> return (grid, gridSize, pcPos, E, stack, Normal)
        2 -> return (grid, gridSize, pcPos, W, stack, Normal)
        3 -> return (grid, gridSize, pcPos, N, stack, Normal)
        4 -> return (grid, gridSize, pcPos, S, stack, Normal)
    '_' -> do
      let (stack', a) = pop stack
      if (a == 0)
        then return (grid, gridSize, pcPos, E, stack', Normal)
        else return (grid, gridSize, pcPos, W, stack', Normal)
    '|' -> do
      let (stack', a) = pop stack
      if (a == 0)
        then return (grid, gridSize, pcPos, S, stack', Normal)
        else return (grid, gridSize, pcPos, N, stack', Normal)
    '"' -> do
      return (grid, gridSize, pcPos, pcDir, stack, String)
    ':' -> return (grid, gridSize, pcPos, pcDir, dup stack, Normal)
    '\\' -> return (grid, gridSize, pcPos, pcDir, swap stack, Normal)
    '$' -> return (grid, gridSize, pcPos, pcDir, tail stack, Normal)
    '.' -> do
      let (stack', a) = pop stack
      putStr $ (show a) ++ " "
      return (grid, gridSize, pcPos, pcDir, stack', Normal)
    ',' -> do
      let (stack', a) = pop stack
      putStr $ (chr a) : ""
      return (grid, gridSize, pcPos, pcDir, stack', Normal)
    '#' -> error "# should already be handled"
    'p' -> do
      let (stack', y) = pop stack
      let (stack'', x) = pop stack'
      let (stack''', val) = pop stack''
      return (put grid (x, y) (chr val), gridSize, pcPos, pcDir, stack''', Normal)
    'g' -> do
      let (stack', y) = pop stack
      let (stack'', x) = pop stack'
      let stack''' = push stack'' (ord $ getSymbol grid (x, y))
      return (grid, gridSize, pcPos, pcDir, stack''', Normal)
    '&' -> do
      line <- getLine
      return (grid, gridSize, pcPos, pcDir, push stack (read line), Normal)
    '~' -> do
      char <- getChar
      return (grid, gridSize, pcPos, pcDir, push stack (ord char), Normal)
    '@' -> error "@ should already be handled"
    _ -> error "Unknown instruction"
evaluate (grid, gridSize, pcPos, pcDir, stack, String) = do
  let symb = getSymbol grid pcPos
  if symb == '"'
    then return (grid, gridSize, pcPos, pcDir, stack, Normal)
    else return (grid, gridSize, pcPos, pcDir, push stack (ord symb), String)
