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
--  part 1 solution: 367059
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day10/aoc_10_test_1.txt"
part_2_test::[Char]
part_2_test = "day10/aoc_10_test_2.txt"
part_3_test::[Char]
part_3_test = "day10/aoc_10_test_3.txt"
part_4_test::[Char]
part_4_test = "day10/aoc_10_test_4.txt"

part_1_input::[Char]
part_1_input = "day10/aoc_10_part_1.txt"
part_2_input::[Char]
part_2_input = "day10/aoc_10_part_2.txt"

closeOf::Char -> Char
closeOf ch
    | '(' == ch = ')'
    | '{' == ch = '}'
    | '[' == ch = ']'
    | '<' == ch = '>'
    | otherwise = ch

openOf::Char -> Char
openOf ch
    | ')' == ch = '('
    | '}' == ch = '{'
    | ']' == ch = '['
    | '>' == ch = '<'
    | otherwise = ch

isOpen::Char->Bool
isOpen ch
    | '(' == ch = True
    | '{' == ch = True
    | '[' == ch = True
    | '<' == ch = True
    | otherwise = False

isClose::Char->Bool
isClose ch
    | ')' == ch = True
    | '}' == ch = True
    | ']' == ch = True
    | '>' == ch = True
    | otherwise = False

match::Char->Char->Bool
match lchar rchar = rchar == (closeOf lchar)

data ParseResult = Corrupted Char Char  | Incomplete [Char] | Ok | Scraps [Char] [Char] [Char] deriving (Show,Eq)

score::Char->Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

scoreCorrupt::ParseResult->Int
scoreCorrupt (Corrupted expect found ) = score found
scoreCorrupt _ = 0

part1::[[Char]]->Int
part1 vals4 = (sum scores)
    where
        result_list = map (\l-> check1 l [] [] ) vals4
        scores = map scoreCorrupt result_list

part2 x = undefined

-- This is ugly but it works for Part 1 
check1::[Char]->[Char]->[Char]->ParseResult
check1 [] [] [] = Ok
check1 [] [] (r:rs) = Incomplete (r:rs)
check1 [] left right = Scraps [] left right
check1 (s:ss) [] [] = check1 ss [s] [closeOf s]
check1 (s:ss) ll@(l:ls) rr@(r:rs) 
    |   s == r      = check1 ss ls rs
    |   isOpen s    = check1 ss (s:ll) ((closeOf s):rr)
    |   s == (closeOf l)   = check1 ss ls rr
    |   otherwise   = Corrupted r s
check1 s l r = Scraps s l r











check [] [] [] = Ok
check [] [] (r:rs) = Incomplete (r:rs)
check [] left right = Scraps [] left right
check (s:ss) [] [] = check ss [s] [closeOf s]
check (s:ss) ll@(l:ls) rr@(r:rs) 
    |   s == r      = check ss ls rs
    |   isOpen s    = check ss (s:ll) ((closeOf s):rr)
    |   s == (closeOf l)   = check ss ls rr
    |   otherwise   = Corrupted r s
check s l r = Scraps s l r

    
        
 




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
            part1_input <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length part1_input)

            vals4 <- getStringVals part_4_test
            printf "    read %d lines of input\n" (length vals4)
   
            let answer1 = part1 part1_input
            printf "\n   Part 1    Solution: %d \n" answer1         

            


            printf "\n\n    done \n \n"

            





      
        