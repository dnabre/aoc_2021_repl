module Main where
import System.Environment
import System.Exit

-- Advent of Code 2021
-- Day 2
--  part 1 solution: 
--  part 2 solution: 

part_1_test = "day2/aoc_02_test_1.txt"
part_2_test = "day2/aoc_02_test_2.txt"

part_1_input = "day2/aoc_02_part_1.txt"
part_2_input = "day2/aoc_02_part_2.txt"

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 0


getIntVals :: FilePath -> IO [Int]
getIntVals path = do 
                    contents <- readFile path
                    return (map (read::String->Int) (lines contents))

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)



main :: IO()
main = do 
            vals1 <- getStringVals part_1_input
            vals2 <- getStringVals part_2_input
            print ( vals1)
            print (part1 vals1)
            exitWith ExitSuccess
            print ( vals2)
            print (part2 vals2)