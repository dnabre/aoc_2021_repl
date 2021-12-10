{-# LANGUAGE MultiWayIf #-}

module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char

-- Advent of Code 2021
-- Day 3
--  part 1 solution: 2003336
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day3/aoc_03_test_1.txt"
part_2_test::[Char]
part_2_test = "day3/aoc_03_test_2.txt"

part_1_input::[Char]
part_1_input = "day3/aoc_03_part_1.txt"
part_2_input::[Char]
part_2_input = "day3/aoc_03_part_2.txt"

count1::[Int] -> Int
count1 xs = if (ones >= zeros) then 1 else 0
    where
        ones = tally 1 xs 0
        zeros = tally 0 xs 0

count0::[Int] -> Int
count0 xs = if (zeros <= ones) then 0 else 1
    where
        ones = tally 1 xs 0
        zeros = tally 0 xs 0




tally:: Int -> [Int] -> Int -> Int
tally _ [] acc = acc
tally value (x:xs) acc = if (value == x)  then (tally value xs (acc+1)) else (tally value xs acc)

gamma_rate_char:: [Char] -> Int
gamma_rate_char xs = gamma_rate (map digitToInt xs) 

gamma_rate:: [Int] -> Int
gamma_rate xs = if (count1 >= count0) then 1 else 0
    where
        count1 = tally 1 xs 0
        count0 = tally 0 xs  0 

epsilon_rate_char :: [Char] -> Int
epsilon_rate_char xs = epsilon_rate (map digitToInt xs)

epsilon_rate:: [Int] -> Int
epsilon_rate xs = if (count1 <= count0) then 1 else 0
    where
        count1 = tally 1 xs 0
        count0 = tally 0 xs  0 

bin2Dec :: [Int] -> Int
bin2Dec = foldl' (\acc x -> acc * 2 + x) 0

bitchar_to_bitlist :: [[Char]] -> [[Int]]
bitchar_to_bitlist = map str2intlist
    where
        str2intlist::[Char] -> [Int]
        str2intlist xs = map digitToInt xs



part1 ::[[Char]] -> (Int,Int,Int)
part1 vals = (g,e,g*e)
    where
        tv = transpose vals
        g_list = map gamma_rate_char tv
        e_list = map epsilon_rate_char tv
        g = bin2Dec g_list
        e = bin2Dec e_list




part2 x = undefined




getIntVals :: FilePath -> IO [Int]
getIntVals path = do 
                    contents <- readFile path
                    return (map (read::String->Int) (lines contents))

getStringVals::FilePath -> IO [[Char]] 
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)


fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) =c

--main :: IO()
main = do 
            printf "Advent of Code 2021, Day 3:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            
            let (_,_,answer1) = part1 vals1                   
            printf "\n    Part 1\n         Solution: %d\n" answer1
          
          --  let answer2 = part2 state_1
          --  printf "    Part 2\n         Solution: %d\n" answer2

            let nvals =  bitchar_to_bitlist vals2
            print nvals
            print (length nvals)
            let bits_by_pos = transpose nvals
            let bit_length = length (head bits_by_pos)
            print bits_by_pos
            print (length bits_by_pos)
            print bit_length
            --have to discard as we go, and redo tally
            printf "\n  o2: %s \n" (show o2)
            printf "\n co2: %s \n" (show co2)


            printf "\n\n    ---  done ---     \n"




