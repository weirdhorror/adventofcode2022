module Day1 where

import Data.List (sort)
import System.IO.Unsafe

inputFilename :: String
inputFilename = "../data/input01.txt"

-- parse lines into groups of integers
parse :: [String] -> [[Integer]]
parse strings = parse' strings [] []

parse' :: [String] -> [Integer] -> [[Integer]] -> [[Integer]]
parse' [] part parts = reverse (part:parts)
parse' ("":xs) [] parts = parse' xs [] parts
parse' ("":xs) part parts = parse' xs [] (reverse part:parts)
parse' (x:xs) part parts = parse' xs (read x:part) parts

-- yolo
text :: String
text = unsafePerformIO . readFile $ inputFilename

elves :: [[Integer]]
elves = parse (lines text)

-- Main

main :: IO ()
main = print (top, topThree)
  where sums = reverse (sort (map sum elves))
        top = head sums
        topThree = sum (take 3 sums)
