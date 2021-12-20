{-# LANGUAGE TypeApplications #-}

module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char
import Data.Massiv.Array
import Data.Massiv.Array.Stencil


-- Advent of Code 2021
-- Day 17
--  part 1 solution: 19503
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day20/aoc_20_test_1.txt"
part_2_test::[Char]
part_2_test = "day20/aoc_20_test_2.txt"
part_1_input::[Char]
part_1_input = "day20/aoc_20_part_1.txt"
part_2_input::[Char]
part_2_input = "day20/aoc_20_part_2.txt"


part1 x = undefined
part2 x = undefined    
        
           
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

splitEmptyLine::[[Char]]->([[Char]],[[Char]])
splitEmptyLine ls = splitEmptyLine' ls []
    where
        splitEmptyLine' ("":xs) p =  (reverse p,xs)
        splitEmptyLine' (x:xs) p = splitEmptyLine' xs (x:p)


main :: IO()
main = do 
            printf "Advent of Code 2021, Day 20:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)

           
            --let answer1 = 0
            --printf "\n   Part 1    Solution: %d \n" answer1         

            
            --let answer2 = part2 
            --printf "\n   Part 2    Solution: %d \n" answer2

            printf "\n---                 done                 ---\n"
