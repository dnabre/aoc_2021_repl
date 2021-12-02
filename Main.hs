module Main where
import Data.List.Split
import Data.Functor

part_1_file = "day1/aoc_01_test_1.txt"
part_2_file = "day1/aoc_01_test_2.txt"
-- map read $ words "1 2 3 4 5" :: [Int]
-- >>= \s -> print s

inputStringList f = splitOn "\n"  <$> readFile part_1_file

--m +Data.Functor
-- length <$> readFile "file.txt"

main = do
  putStrLn (inputStringList part_1_file)
