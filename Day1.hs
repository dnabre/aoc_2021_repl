module Day1 where

-- Advent of Code 2021
-- Day 1
--  part 1 solution: 1553
--  part 2 solution: 1597



part_1_test = "day1/aoc_01_test_1.txt"
part_2_test = "day1/aoc_01_test_2.txt"

part_1_input = "day1/aoc_01_part_1.txt"
part_2_input = "day1/aoc_01_part_2.txt"

part1 :: [Int] -> Int
part1 (x:xs) = sum $ map increase pairs
    where   
        pairs = zip (x:xs) xs
        increase (a, b) = if(a < b) then 1 else 0

--part2 :: [Int] -> Int
part2 xs = part1 $ avg xs
    where
        z3 ls = zip3 ls (tail ls) (tail (tail ls))
        avg ls = map (\(a,b,c) -> a+b+c) (z3 ls)
getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (lines contents))


main :: IO()
main = do 
            vals1 <- getVals part_1_input
            vals2 <- getVals part_2_input
            print (part1 vals1)
            print (part2 vals2)