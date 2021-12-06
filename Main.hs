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
--  part 2 solution: 1687617803407

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
        



do_n_days state 0 = state
do_n_days state n = do_n_days (nextDay state) (n-1)

do_count_step (f0, f1, f2, f3, f4, f5, f6, f7, f8) =  (f1,f2,f3,f4,f5,f6,f7+f0,f8,f0)

do_count_n_days state 0 = state
do_count_n_days state n = do_count_n_days (do_count_step state) (n-1)

tally [] state = state
tally (0:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =  tally xs (f0+1, f1, f2, f3, f4, f5, f6, f7, f8)
tally (1:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1+1, f2, f3, f4, f5, f6, f7, f8)
tally (2:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2+1, f3, f4, f5, f6, f7, f8)
tally (3:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3+1, f4, f5, f6, f7, f8)
tally (4:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4+1, f5, f6, f7, f8)
tally (5:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5+1, f6, f7, f8)
tally (6:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5, f6+1, f7, f8)
tally (7:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5, f6, f7+1, f8)
tally (8:xs) (f0, f1, f2, f3, f4, f5, f6, f7, f8) =   tally xs (f0, f1, f2, f3, f4, f5, f6, f7, f8+1)


tup_to_list (f0, f1, f2, f3, f4, f5, f6, f7, f8) = [f0, f1, f2, f3, f4, f5, f6, f7, f8]


main :: IO()
main = do 
            printf "Advent of Code 2021, Day 6:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)
            let nvals = (map string2int (splitOn "," (head vals1)))
          
            let n = 80
            let state_1 = tally nvals (0,0,0,0,0,0,0,0,0)
            let after_80 = do_count_n_days state_1 n
            print (sum (tup_to_list after_80))

            let n2 = 256
            let after_256 = do_count_n_days state_1 n2
            print (sum (tup_to_list after_256))



            printf "\n\n   done  \n\n"