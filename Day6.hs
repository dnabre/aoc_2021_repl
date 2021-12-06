module Main where

import Text.Printf
import Data.List.Split

-- Advent of Code 2021
-- Day 6
--  part 1 solution: 374927
--  part 2 solution: 1687617803407
part_1_test::[Char]
part_1_test = "day6/aoc_06_test_1.txt"

part_2_test::[Char]
part_2_test = "day6/aoc_06_test_2.txt"

part_1_input::[Char]
part_1_input = "day6/aoc_06_part_1.txt"

part_2_input::[Char]
part_2_input = "day6/aoc_06_part_2.txt"

part1::(Int, Int, Int, Int, Int, Int, Int, Int, Int)->Int
part1 state = sum $ tup_to_list (do_count_n_days state 80)
part2::(Int, Int, Int, Int, Int, Int, Int, Int, Int)->Int
part2 state = sum $ tup_to_list (do_count_n_days state 256)


getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)
string2int::[Char]->Int
string2int = r
    where 
        r:: [Char] -> Int
        r = read

rowString2list :: [Char] -> [Int]
rowString2list xs = map string2int (filter (\x->x /= "") $ splitOn " " xs)

do_count_step::(Int, Int, Int, Int, Int, Int, Int, Int, Int)->(Int, Int, Int, Int, Int, Int, Int, Int, Int)
do_count_step (f0, f1, f2, f3, f4, f5, f6, f7, f8) =  (f1,f2,f3,f4,f5,f6,f7+f0,f8,f0)

do_count_n_days::(Int, Int, Int, Int, Int, Int, Int, Int, Int)->Int->(Int, Int, Int, Int, Int, Int, Int, Int, Int)
do_count_n_days state 0 = state
do_count_n_days state n = do_count_n_days (do_count_step state) (n-1)

tally::[Int]->(Int, Int, Int, Int, Int, Int, Int, Int, Int)->(Int, Int, Int, Int, Int, Int, Int, Int, Int)
tally [] state = state
tally (0:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =  tally xs (f0+1, f1, f2, f3, f4, f5, f6, f7, f8)
tally (1:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1+1, f2, f3, f4, f5, f6, f7, f8)
tally (2:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2+1, f3, f4, f5, f6, f7, f8)
tally (3:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3+1, f4, f5, f6, f7, f8)
tally (4:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4+1, f5, f6, f7, f8)
tally (5:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5+1, f6, f7, f8)
tally (6:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5, f6+1, f7, f8)
tally (7:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5, f6, f7+1, f8)
tally (8:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5, f6, f7, f8+1)

tup_to_list :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> [Int]
tup_to_list (f0, f1, f2, f3, f4, f5, f6, f7, f8) = [f0, f1, f2, f3, f4, f5, f6, f7, f8]

main :: IO()
main = do
            printf "Advent of Code 2021, Day 6:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)
            let nvals = (map string2int (splitOn "," (head vals1)))
            let state_1 = tally nvals (0,0,0,0,0,0,0,0,0)
            
            let answer1 = part1 state_1                    
            printf "\n    Part 1\n         Solution: %d\n" answer1
            let answer2 = part2 state_1
            printf "    Part 2\n         Solution: %d\n" answer2
         