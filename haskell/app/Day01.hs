module Day01 (main) where

import Helpers (yoloReadFile)
import Data.List (sort)

inputData :: String
inputData = yoloReadFile "../data/input01.txt"

type Calorie = Integer
type Elf = [Calorie]
type Elves = [Elf]

-- parse lines into groups of integers
parse :: [String] -> Elves
parse strings = parse' strings [] []

parse' :: [String] -> Elf -> Elves -> Elves
parse' [] elf elves = reverse (elf:elves)
parse' ("":xs) [] elves = parse' xs [] elves
parse' ("":xs) elf elves = parse' xs [] (reverse elf:elves)
parse' (x:xs) elf elves = parse' xs (read x:elf) elves


main :: IO ()
main = print (top, topThree)
  where elves = parse (lines inputData)
        sums = reverse (sort (map sum elves))
        top = head sums
        topThree = sum (take 3 sums)
