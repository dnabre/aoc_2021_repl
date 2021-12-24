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



openOf ch
    | '(' == ch = ')'
    | '{' == ch = '}'
    | '[' == ch = ']'
    | '<' == ch = '>'
    | otherwise = ch

closeOf ch
    | ')' == ch = '('
    | '}' == ch = '{'
    | ']' == ch = '['
    | '>' == ch = '<'
    | otherwise = ch

isOpen ch
    | '(' == ch = True
    | '{' == ch = True
    | '[' == ch = True
    | '<' == ch = True
    | otherwise = False

isClose ch
    | ')' == ch = True
    | '}' == ch = True
    | ']' == ch = True
    | '>' == ch = True
    | otherwise = False


match lchar rchar = rchar == (closeOf lchar)

data ParseResult = Corrupted Char Char | Incomplete Char | Ok | Scraps [Char] [Char] [Char] deriving (Show,Eq)

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

scoreCorrupt (Corrupted expect found ) = score found

part1 x = undefined



part2 x = undefined


check [] left right = Scraps [] left right
check (s:ss) left right = if (isOpen s) 
                            then          check ss (s:left) ((closeOf s):right)
{-check [] (x:xs) (y:ys) = if (y == (closeOf x))
                                then check [] xs ys
                                else Corrupted (closeOf x) y
-}
--check [] (x:xs) [] = Incomplete (closeOf x)
--check s l r = Scraps s l r




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

            let first = (head vals2)
            print first
            let f_result = check first [] [] 
            print f_result


            printf "\n\n    done \n \n"





      
        