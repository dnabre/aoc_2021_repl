module Main where

import Text.Printf
import Data.List.Split
import Data.List

-- Advent of Code 2021
-- Day 8
--  part 1 solution: 
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day8/aoc_08_test_1.txt"
part_2_test::[Char]
part_2_test = "day8/aoc_08_test_2.txt"
part_1_input::[Char]
part_1_input = "day8/aoc_08_part_1.txt"
part_2_input::[Char]
part_2_input = "day8/aoc_08_part_2.txt"

part1 x = undefined

part2 x = undefined


tailless ls = reverse (drop 1 (reverse ls))

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

string2int::[Char]->Int
string2int = r
    where 
        r:: [Char] -> Int
        r = read


main :: IO()
main = do
            printf "Advent of Code 2021, Day 8:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            
            let words =  splitOn " " (head vals1)
            let input = tailless $ head words
            let output = head (tail words)
            
            print vals1

    

           



            print "done"