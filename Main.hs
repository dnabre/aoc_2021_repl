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


data R = R Int Int  deriving (Eq,Show)
data S = S Int Int  deriving (Eq,Show)
data C = C Int Int  deriving (Eq,Show)
data A = A Int Int  deriving (Eq,Show)

instance Monoid R where
    mempty = R 0 0 

instance Semigroup R where
    (R a b) <> (R c d) 
        | b <= c = R ( a + c - b) d
        | otherwise = R a (d + b - c)
--  --------- end R ---------  --

instance Monoid S where
    mempty = S 0 0 


instance Semigroup S where
    (S a b) <> (S c d) 
        | b <= c = S ( a + c - b) d
        | otherwise = S a (d + b - c)
--  --------- end S ---------  --

instance Monoid C where
    mempty = C 0 0 

instance Semigroup C where
    (C a b) <> (C c d) 
        | b <= c = C ( a + c - b) d
        | otherwise = C a (d + b - c)
--  --------- end C ---------  --


instance Monoid A where
    mempty = A 0 0 

instance Semigroup A where
    (A a b) <> (A c d) 
        | b <= c = A ( a + c - b) d
        | otherwise = A a (d + b - c)

--  --------- end A ---------  --


data Pairing = Paren , SBrace, CBrace, ABrace

data Closing = Closing  (Seq Token) 


parse1 '(' = R 0 1
parse1 ')' = R 1 0
parse1 _   = R 0 0 

parse2 '[' = S 0 1
parse2 ']' = S 1 0
parse2 _   = S 0 0 

parse3 '{' = C 0 1
parse3 '}' = C 1 0
parse3 _   = C 0 0 


parse4 '<' = A 0 1
parse4 '>' = A 1 0
parse4 _   = A 0 0 




balanced1 xs = foldMap parse1 xs ==  R 0 0
balanced2 xs = foldMap parse2 xs ==  S 0 0
balanced3 xs = foldMap parse3 xs ==  C 0 0
balanced4 xs = foldMap parse4 xs ==  A 0 0 

balanced xs = (balanced1 xs, balanced2 xs, balanced3 xs, balanced4 xs)

isGood xs = good (balanced xs)
     

good (True, True, True, True) = True
good _  = False

-- based on "https://www.youtube.com/watch?v=Txf7swrcLYs" , not sure what I'm doing

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
        