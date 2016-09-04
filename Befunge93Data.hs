module Befunge93Data where

data PCDir = N 
           | S 
           | E 
           | W

type PCPos = (Int, Int)

type GridSize = (Int, Int)

type Stack = [Int]

type Grid = [[Char]]

type Program = (Grid,
                GridSize, 
                PCPos, 
                PCDir, 
                Stack)

pop :: Stack -> (Stack, Int)
pop (i:is) = (is, i)
pop []   = error "Popping empty stack"

push :: Stack -> Int -> Stack
push s i = i:s

swap :: Stack -> Stack
swap (a:(b:s)) = b:(a:s)
swap _         = error "Swapping too small stack"