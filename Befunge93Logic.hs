module Befunge93Logic where

import Befunge93Data

fungify :: String -> Maybe Grid
fungify file = 
  if (width > 80 || height > 25) 
    then Nothing 
    else Just file''
  where file'  = lines file
        width  = findLongestLine file'
        file'' = padLines file' width
        height = length file'

findLongestLine :: [String] -> Int
findLongestLine lines = maximum (map (length) lines)

padLines :: [String] -> Int -> [String]
padLines lines size = map (padLine size) lines

padLine :: Int -> String -> String
padLine size line = line ++ (replicate (size - (length line)) ' ')