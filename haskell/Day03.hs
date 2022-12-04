module Day03 where
import Data.Char (ord)
import Data.Ix (inRange)
import Data.List.Split (chunksOf)
import Data.Set (toList, fromList)
import System.IO.Unsafe

inputData :: String
inputData = unsafePerformIO . readFile $ "../data/input03.txt"

sampleData :: String
sampleData = unsafePerformIO . readFile $ "../data/input03-sample.txt"

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

unique :: [Char] -> [Char]
unique list = toList $ fromList list

hasChar :: Char -> [Char] -> Bool
hasChar char word = char `elem` word

priority :: Char -> Int
priority char | inRange (97, 122) code = code - 96
              | inRange (65, 90) code = code - 38
              | otherwise = -1
  where code = ord char

firstSharedChar :: [String] -> Char
firstSharedChar x = head (sharedChars x)

sharedChars :: [String] -> [Char]
sharedChars [] = []
sharedChars (x:xs) = sharedChars' first rest []
  where first = unique x
        rest = map unique xs

sharedChars' :: String -> [String] -> [Char] -> [Char]
sharedChars' [] _ shared = shared
sharedChars' (x:xs) ys shared
  | all (hasChar x) ys = sharedChars' xs ys (x:shared)
  | otherwise = sharedChars' xs ys shared

splitPair :: String -> [String]
splitPair string = [left, right]
  where len = length string
        mid = div len 2
        left = slice 0 (mid - 1) string
        right = slice mid len string

rucksacks :: [String]
rucksacks = lines inputData

partOne :: Int
partOne = total
  where pairs = map splitPair rucksacks
        shared = map firstSharedChar pairs
        priorities = map priority shared
        total = sum priorities

partTwo :: Int
partTwo = total
  where groups = chunksOf 3 rucksacks
        shared = map firstSharedChar groups
        priorities = map priority shared
        total = sum priorities

-- Main

main :: IO ()
main = print (partOne, partTwo)
