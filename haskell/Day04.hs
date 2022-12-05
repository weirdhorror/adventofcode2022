module Day04 where
import Data.List.Split (splitOn)
import System.IO.Unsafe

inputData :: String
inputData = unsafePerformIO . readFile $ "../data/input04.txt"

sampleData :: String
sampleData = unsafePerformIO . readFile $ "../data/input04-sample.txt"

type Range = (Int, Int)

anyOverlap :: Range -> Range -> Bool
anyOverlap (a, b) (x, y)
  | x < a = anyOverlap' (x, y) (a, b)
  | otherwise = anyOverlap' (a, b) (x, y)

anyOverlap' :: Range -> Range -> Bool
anyOverlap' (a, b) (x, y)
  | a <= x && b >= x = True
  | b >= y = True
  | otherwise = False

eitherContained :: Range -> Range -> Bool
eitherContained (a, b) (x, y)
  | a >= x && b <= y  = True
  | a <= x &&  b >= y = True
  | otherwise = False

parseRanges :: String -> [Range]
parseRanges line = ranges
  where dashedRanges = splitOn "," line              -- ["2-4","6-8"]
        rangePairs = map (splitOn "-") dashedRanges  -- [["2","4"],["6","8"]]
        ranges = map parseRange rangePairs           -- [(2,4),(6,8)]

parseRange :: [String] -> Range
parseRange [left, right] = (read left, read right)
parseRange _ = error "range requires two elements"

parsePair :: (Range -> Range -> Bool) -> [Range] -> Bool
parsePair f [leftPair, rightPair] = f leftPair rightPair
parsePair _ _ = error "pair requires two ranges"

-- Main

main :: IO ()
main = print (total1, total2)
  where inputLines = lines inputData
        ranges = map parseRanges inputLines
        contains = map (parsePair eitherContained) ranges
        -- overlaps = map (\r -> (r, parsePair anyOverlap r)) ranges
        overlaps = map (parsePair anyOverlap) ranges
        total1 = length (filter id contains)
        total2 = length (filter id overlaps)
