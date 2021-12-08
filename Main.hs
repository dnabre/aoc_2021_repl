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

fuel2Cost::Int -> Int -> Int
fuel2Cost n t = div (x*x + x )2
    where 
        x = abs (n - t)
fuelCost:: Int -> Int -> Int
fuelCost n t = abs ( n - t )

totalfuelCost:: [Int] -> Int -> Int
totalfuelCost ls t = sum (map (fuelCost t) ls) 

totalfuelCost2:: [Int] -> Int -> Int
totalfuelCost2 ls t = sum (map (fuel2Cost t) ls)

reverse_total_fuel_cost_2:: Int -> [Int] -> [Int] -> Int
reverse_total_fuel_cost_2 f ls (x:[]) = x
reverse_total_fuel_cost_2 f ls (x:xs) = if (f == (totalfuelCost2 ls f)) then x else reverse_total_fuel_cost_2 f ls xs


average::[Int]->Int
average ls = div (sum ls)  (length ls)

getMiddle::[Int]->Int
getMiddle [] = 0
getMiddle xs = (a' + b') `div` 2
    where a' = head $ drop a xs
          b' = head $ drop b xs
          a = (n `div` 2)
          b = n' - 1
          n' = n `div` 2
          n = length xs

median::[Int]->Int
median [] = 0
median xs = result
    where result = if (n `mod` 2 == 0)
                    then getMiddle sorted
                    else head $ drop a sorted
          a = (n - 1) `div` 2
          n = length xs
          sorted = sort xs

min_pair::([Int],[Int])->(Int,Int)
min_pair ((x:xs),(y:ys)) = min_pair_h xs ys (x,y)
    where
        min_pair_h [] [] (mn, v) = (mn,v)
        min_pair_h (x:xs) (y:ys) (mn,v) = if (x < mn) then min_pair_h xs ys (x,y) else min_pair_h xs ys  (mn,v) 
        
main :: IO()
main = do
            printf "Advent of Code 2021, Day 7:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
         --   printf "    read %d lines of input\n" (length vals2)
            let nvals = (map string2int (splitOn "," (head vals1)))
            print nvals
           
            print (totalfuelCost nvals 1)
            print (totalfuelCost nvals 3)
            print (totalfuelCost nvals 10)
           
            let nvals2 = (map string2int (splitOn "," (head vals2)))
            print nvals2
            let ls = nvals2
            let min = minimum ls
            let max = maximum ls
            let med = median ls
         
            printf "\n   min   : %d \n   max   : %d\n" min max
      
            printf "   median: %d \n" med  
            --mapM_ print    [-5..5]  
            let fuels2 = map (\x-> (x,totalfuelCost2  ls x)) [min..max]
            let f4 = map (totalfuelCost2 ls) [min..max]
            let f5 = sort f4
           -- mapM_ print fuels2
            --  105 780 131 too high
            print (take 5 f5)
            let answer = head f5
            print answer
            let aa=  (reverse_total_fuel_cost_2 answer ls ls)
            print aa
            printf "\ntotal fuel cost for target %d is %d (%d)\n" aa (totalfuelCost2 ls aa) answer
            let q = [q| q <- [min .. max], answer == (totalfuelCost2 ls q)]
            print q
        --    let answer1 = part1 state_1                    
        --   printf "\n    Part 1\n         Solution: %d\n" answer1
        --    let answer2 = part2 state_1
        --    printf "    Part 2\n         Solution: %d\n" answer2
            print "done"
         