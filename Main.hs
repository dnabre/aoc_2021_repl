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
-- Day 5
--  part 1 solution: 
--  part 2 solution: 

part_1_test = "day5/aoc_05_test_1.txt"
part_2_test = "day5/aoc_05_test_2.txt"

part_1_input = "day5/aoc_05_part_1.txt"
part_2_input = "day5/aoc_05_part_2.txt"

x_max = 1000
y_max = 1000

data Line=Line (Int,Int) (Int, Int)|LineError deriving Show

-- look at [1..x_max] * [1.._y_max] 
-- for each point, count # lines it hits. if count>=2 -> 1 else 0. total
-- alternately, only enumerate points on lines. check if already enumerated, if so tally hit 
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
    
parseLineInt ps = map (\x->map string2int x) ps


parsePairs (p1:p2:rs) = parseLine (p1,p2)
parsePairs _ = LineError   

parseLine ( (x1:y1:r1), (x2:y2:r2) ) = Line (x1,y1) (x2, y2)
parseLine _ = LineError


horzLines (Line (x1, y1) (x2, y2)) = (x1 == x2)
horzLines (LineError) = False

vertLines (Line (x1, y1) (x2, y2)) = (y1 == y2)
vertLines (LineError) = False

main :: IO()
main = do 
            printf "Advent of Code 2021, Day 5:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            
            print vals1
            let lls = map (splitOn "->") vals1
            let llp = map (\x->map (splitOn ",") x) lls
            let int_lines = map parseLineInt llp
           
            let line_list = map parsePairs int_lines 
            print line_list
            print (length line_list)
            let h_lines = (filter horzLines line_list)
            let v_lines = (filter vertLines line_list)
            printf "\n hort: %d vert: %d \n" (length h_lines) (length v_lines)
          --  printf "\n    Part 2\n      Solution: %d \n" 0









            printf "\n\n   done  \n\n"