module Befunge93Data where

data PCDir = N 
           | S 
           | E 
           | W
  deriving Show

data Mode = String
          | Normal
  deriving Show

type PCPos = (Int, Int)

type GridSize = (Int, Int)

type Stack = [Int]

type Grid = [[Char]]

type Program = (Grid,
                GridSize, 
                PCPos, 
                PCDir, 
                Stack,
                Mode)

pop :: Stack -> (Stack, Int)
pop (i:is) = (is, i)
pop []   = ([], 0)

push :: Stack -> Int -> Stack
push s i = i:s

swap :: Stack -> Stack
swap (a:(b:s)) = b:(a:s)
swap _         = error "Swapping too small stack"

dup :: Stack -> Stack
dup (a:as) = a:(a:as)
dup _      = []