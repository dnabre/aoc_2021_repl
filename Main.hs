module Main where

part_1_test = "day1/aoc_01_test_1.txt"
part_2_test = "day1/aoc_01_test_2.txt"

part_1_input = "day1/aoc_01_part_1.txt"
part_2_input = "day1/aoc_01_part_2.txt"

part1 :: [Int] -> Int
part1 (x:xs) = sum $ map increase pairs
    where   
        pairs = zip (x:xs) xs
        increase (a, b) = if(a < b) then 1 else 0

part2 :: [Int] -> Int
part2 _ = 0


getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (lines contents))


main :: IO()
main = do vals <- getVals part_1_input
          print (part1 vals)
          print (part2 vals)