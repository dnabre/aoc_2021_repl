module Main where

import Text.Printf
import Data.List.Split
import Data.List
import Data.Char



-- Advent of Code 2021
-- Day 24
--  part 1 solution: 2657
--  part 2 solution: 2911561572630

part_1_test::[Char]
part_1_test = "day24/aoc_24_test_1.txt"
part_2_test::[Char]
part_2_test = "day24/aoc_24_test_2.txt"
part_1_input::[Char]
part_1_input = "day24/aoc_24_part_1.txt"
part_2_input::[Char]
part_2_input = "day24/aoc_24_part_2.txt"

data Var= VarW| VarX | VarY | VarZ deriving (Show,Eq,Ord,Enum)
data Instruction =  Inp Var     | 
                    Add Var Int |
                    Mul Var Int |
                    Div Var Int |
                    Mod Var Int |
                    Eql Var Int deriving (Show,Eq,Ord,Enum)

part1 x = undefined
part2 x = undefined

SetState VarW n (w,x,y,z) = (n,x,y,z)
SetState VarX n (w,x,y,z) = (w,n,y,z)
SetState VarU n (w,x,y,z) = (w,x,n,z)
SetState VarZ n (w,x,y,z) = (w,x,y,n)

GetState VarW (w,x,y,z) = w 
GetState VarX (w,x,y,z) = x
GetState VarU (w,x,y,z) = y
GetState VarZ (w,x,y,z) = z


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
            printf "Advent of Code 2021, Day 24:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)
            print vals1
      
{-      
            let answer1 = vals1
            printf "\n   Part 1    Solution: %d \n" answer1         

          
            let answer2 = part2 vals2
            printf "\n   Part 2    Solution: %d \n" answer2
-}
            printf "\n\n done\n\n"

