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
--  part 1 solution: 
--  part 2 solution: 

part_1_test = "day3/aoc_03_test_1.txt"
part_2_test = "day3/aoc_03_test_2.txt"

part_1_input = "day3/aoc_03_part_1.txt"
part_2_input = "day3/aoc_03_part_2.txt"

--tally::Char -> [Char] -> Int -> Int
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

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)



fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) =c

--main :: IO()
main = do 
            printf "Advent of Code 2021, Day 3:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            print vals1
            print (transpose vals1)
            let g_list = map gamma_rate_char (transpose vals1)
            let e_list = map epsilon_rate_char (transpose vals1)
            printf "gamma: "
            print g_list
            print (bin2Dec g_list)
            printf "epsilon: "
            print e_list
            print (bin2Dec e_list)
            return vals1
        --    let program1 =  (map parseInstruction ( map instructParts vals1)) ++ [Stop]
        --    let final_state_1 = (part1 program1)
        --    let answer_part_1 = (\(x,y) -> x*y) final_state_1
        
        -- printf "   Part 1 \n     final location (%d,%d) -> %d\n" (fst final_state_1) (snd final_state_1) answer_part_1


          --  let program2 =  (map parseInstruction ( map instructParts vals2)) ++ [Stop]
          --  let final_state_2 = (part2 program2)
          --  let answer_part_2 = (\(x,y,_) -> x*y) final_state_2

          --  printf "   Part 2 \n     final location (%d,%d,%d) -> %d\n" (fst3 final_state_2) (snd3 final_state_2) ( thrd3 final_state_2) answer_part_2
