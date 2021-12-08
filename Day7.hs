module Main where

import Text.Printf
import Data.List.Split
import Data.List

-- Advent of Code 2021
-- Day 7
--  part 1 solution: 339321
--  part 2 solution: 95476244

part_1_test::[Char]
part_1_test = "day7/aoc_07_test_1.txt"
part_2_test::[Char]
part_2_test = "day7/aoc_07_test_2.txt"
part_1_input::[Char]
part_1_input = "day7/aoc_07_part_1.txt"
part_2_input::[Char]
part_2_input = "day7/aoc_07_part_2.txt"

part1::[Int]->Int
part1 ls = snd $ solve ls totalfuelCost

part2::[Int]->Int
part2 ls = snd $ solve ls totalfuelCost2

solve :: [Int] -> ([Int] -> Int -> Int) -> (Int,Int)
solve ls fuel = head $ sortOn snd (zip ls (map (fuel ls) range))
    where 
        range = [(minimum ls) .. (maximum ls)]

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

string2int::[Char]->Int
string2int = r
    where 
        r:: [Char] -> Int
        r = read

fuelCost:: Int -> Int -> Int
fuelCost n t = abs ( n - t )

fuelCost2::Int -> Int -> Int
fuelCost2 n t = div (x*x + x )2
    where 
        x = abs (n - t)

totalfuelCost:: [Int] -> Int -> Int
totalfuelCost ls t = sum (map (fuelCost t) ls) 

totalfuelCost2:: [Int] -> Int -> Int
totalfuelCost2 ls t = sum (map (fuelCost2 t) ls)

main :: IO()
main = do
            printf "Advent of Code 2021, Day 7:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)

            let nvals1 = (map string2int (splitOn "," (head vals1)))
            let answer1 = part1 nvals1
            printf "\n    Part 1\n         Solution: %d\n" answer1

            let nvals2 = (map string2int (splitOn "," (head vals2)))
            let answer2 = part2 nvals2
            printf "    Part 2\n         Solution: %d\n" answer2