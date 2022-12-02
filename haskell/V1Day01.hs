module V1Day1 where

import Data.List
import System.IO.Unsafe

inputFilename :: String
inputFilename = "../data/input01.txt"

-- split string by delimiter of any length
split :: String -> String -> [String]
split delim string = split' delim string [] []

split' :: String -> String -> String -> [String] -> [String]
split' _ [] part parts = reverse (reverse part:parts)
split' delim (x:xs) part parts
  | isDelimPrefix = split' delim sansDelim [] (reverse part:parts)
  | otherwise = split' delim xs (x:part) parts
  where isDelimPrefix = delim `isPrefixOf` (x:xs)
        sansDelim = stripPrefix' delim (x:xs)

-- return string with prefix removed if present
stripPrefix' :: String -> String -> String
stripPrefix' prefix string =
  case stripPrefix prefix string of
     Just rest -> rest
     Nothing -> string

-- yolo
text :: String
text = unsafePerformIO . readFile $ inputFilename

parseGroup :: String -> [Integer]
parseGroup chunk = map read (lines chunk)

elves :: [[Integer]]
elves = map parseGroup (split "\n\n" text)

-- Main

main :: IO ()
main = print mostCalories
  where mostCalories = maximum (map sum elves)
