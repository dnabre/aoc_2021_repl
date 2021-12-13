{-# LANGUAGE MultiWayIf #-}

module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char

-- Advent of Code 2021
-- Day 10
--  part 1 solution: 
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day10/aoc_10_test_1.txt"
part_2_test::[Char]
part_2_test = "day10/aoc_10_test_2.txt"

part_1_input::[Char]
part_1_input = "day10/aoc_10_part_1.txt"
part_2_input::[Char]
part_2_input = "day10/aoc_10_part_2.txt"




part1 x = undefined



part2 x = undefined




getIntVals :: FilePath -> IO [Int]
getIntVals path = do 
                    contents <- readFile path
                    return (map (read::String->Int) (lines contents))

getStringVals::FilePath -> IO [[Char]] 
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)





main = do 
            printf "Advent of Code 2021, Day 10:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            
            print vals1
            let l1 = (head vals1)
            print l1
            print (length l1)
            let b1 = map balanced vals1
            print b1
            let g1 = map isGood vals1
            print g1
        