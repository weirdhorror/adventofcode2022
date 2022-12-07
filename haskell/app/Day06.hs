module Day06 (main, slice, allUnique, findMarker) where

import qualified Data.Set as Set
import Helpers (yoloReadFile)

inputData :: String
inputData = yoloReadFile "../data/input06.txt"

slice :: Int -> Int -> String -> String
slice from to xs = take (to - from + 1) (drop from xs)

allUnique :: String -> Bool
allUnique str = Set.size (Set.fromList str) == length str

findMarker :: String -> Int -> Maybe (String, Int)
findMarker str seqLen = findMarker' str seqLen seqLen

findMarker' :: String -> Int -> Int -> Maybe (String, Int)
findMarker' str seqLen index
  | index >= length str = Nothing
  | allUnique chunk = Just (chunk, index)
  | otherwise = findMarker' str seqLen (index + 1)
  where chunk = slice (index - seqLen) (index - 1) str

main :: IO ()
main = print (packetMarker, messageMarker)
  where packetMarker = findMarker inputData 4
        messageMarker = findMarker inputData 14
