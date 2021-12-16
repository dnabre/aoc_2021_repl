module Main where

import Text.Printf
import Data.List
import Data.Char

-- Advent of Code 2021
-- Day 3
--  part 1 solution: 2003336
--  part 2 solution: 1877139

part_1_test::[Char]
part_1_test = "day3/aoc_03_test_1.txt"
part_2_test::[Char]
part_2_test = "day3/aoc_03_test_2.txt"

part_1_input::[Char]
part_1_input = "day3/aoc_03_part_1.txt"
part_2_input::[Char]
part_2_input = "day3/aoc_03_part_2.txt"

countOnes :: Int -> [[Char]] -> Int
countOnes idx ls = length $ filter (== '1') $ map (!! idx) ls

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

charBin2Dec::[Char]->Int
charBin2Dec ss = bin2Dec (head (bitchar_to_bitlist [ss]))

getRating::Int->[[Char]]->(Char,Char)->[Char]
getRating _ [x] _ = x
getRating n ls bits = getRating (n+1)  next_lines bits
    where
        (ll,rr) = bits
        ones = countOnes n ls
        fewBits = if (2*ones) >= (length ls) then ll else rr
        next_lines = filter (\s-> (s !! n) == fewBits ) ls

part1 ::[[Char]] -> (Int,Int,Int)
part1 vals = (g,e,g*e)
    where
        tv = transpose vals
        g_list = map gamma_rate_char tv
        e_list = map epsilon_rate_char tv
        g = bin2Dec g_list
        e = bin2Dec e_list

part2::[[Char]]->Int
part2 ls = product (map charBin2Dec ratings)
    where
        ratings = map (\b->getRating 0 ls b) [('1','0'),('0','1')]         

getStringVals::FilePath -> IO [[Char]] 
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

main::IO ()
main = do 
            printf "Advent of Code 2021, Day 3:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            ls <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length ls)
            
            let (_,_,answer1) = part1 vals1                   
            printf "\n    Part 1\n         Solution: %d\n" answer1
          
            let answer2 = part2 ls

            printf "    Part 2\n         Solution: %d\n" answer2





