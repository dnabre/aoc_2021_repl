module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

-- Advent of Code 2021
-- Day 6
--  part 1 solution: 374927
--  part 2 solution: 

part_1_test = "day6/aoc_06_test_1.txt"
part_2_test = "day6/aoc_06_test_2.txt"

part_1_input = "day6/aoc_06_part_1.txt"
part_2_input = "day6/aoc_06_part_2.txt"



part1 x = undefined


part2 x = undefined



getIntVals :: FilePath -> IO [Int]
getIntVals path = do 
                    contents <- readFile path
                    return (map (read::String->Int) (lines contents))

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
    
nextDay fs = concat $ map nextFish fs
    where
        nextFish 0 = [6,8]
        nextFish 1 = [0]
        nextFish 2 = [1]
        nextFish 3 = [2]
        nextFish 4 = [3]
        nextFish 5 = [4]
        nextFish 6 = [5]
        nextFish 7 = [6]
        nextFish 8 = [7]
        nextFish _ = []
        

--next state 19  = []
--next state n  = do
--                    printf "After %d days: %s \n" n state


do_n_days state 0 = state
do_n_days state n = do_n_days (nextDay state) (n-1)




main :: IO()
main = do 
            printf "Advent of Code 2021, Day 6:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            let nvals = (map string2int (splitOn "," (head vals1)))
            printf "\n   Initial state: %s\n" (show nvals)
            let n = 256
            let e_vals = do_n_days nvals n
            printf "\n    After  days: " 
            print n
        --    print e_vals
            printf "number: "
            print $ length e_vals
            










            printf "\n\n   done  \n\n"