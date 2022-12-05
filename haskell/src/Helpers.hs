module Helpers (yoloReadFile) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO.Unsafe

yoloReadFile :: FilePath -> String
yoloReadFile filename = trim fileData
  where fileData = unsafePerformIO . readFile $ filename

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = dropWhileEnd isSpace

trim :: String -> String
trim = trimRight . trimLeft
