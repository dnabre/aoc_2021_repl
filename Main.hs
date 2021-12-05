{-# LANGUAGE MultiWayIf #-}

module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.IntSet as IntSet

-- Advent of Code 2021
-- Day 4
--  part 1 solution: 64084
--  part 2 solution: 

part_1_test = "day4/aoc_04_test_1.txt"
part_2_test = "day4/aoc_04_test_2.txt"

part_1_input = "day4/aoc_04_part_1.txt"
part_2_input = "day4/aoc_04_part_2.txt"

set_sum::IntSet.IntSet->Int
set_sum iset = sum (IntSet.toList iset)

part1 vals = undefined





part2 x = undefined

split_boards:: [[Int]] -> [[[Int]]]
split_boards xs = split_boards_h xs [] 

split_boards_h:: [[Int]] -> [[[Int]]] -> [[[Int]]]
split_boards_h [] acc = acc
split_boards_h (x:xs) acc = if (x == []) then  split_boards_h (drop 5 xs) ((mkBoard xs):acc) else []
    where
        mkBoard xs = (take 5 xs)

mkBoard:: [[Int]]->[IntSet.IntSet]
mkBoard xs = (map IntSet.fromList xs) ++ (map IntSet.fromList (transpose xs))

isWin::IntSet.IntSet -> [IntSet.IntSet] -> IntSet.IntSet
isWin pulls [] = IntSet.empty
isWin pulls (b:bs) = if (IntSet.isSubsetOf b pulls) then b else isWin pulls bs


scoreBoard::[IntSet.IntSet]->IntSet.IntSet->Int->Int
scoreBoard board pulls last_pull = (set_sum unmarked) * last_pull  
    where
        unmarked = IntSet.difference  (IntSet.unions board) pulls

getWinningBoard::IntSet.IntSet->[[IntSet.IntSet]]->[IntSet.IntSet]
getWinningBoard plist boards = head [b | b<-boards, ((isWin plist b) /= IntSet.empty)]
    


final_pull_list::[Int]->[[IntSet.IntSet]]->IntSet.IntSet->(IntSet.IntSet,Int)
final_pull_list (p:ps) board_list running_pulls = if ((sum line_sizes) > 0) then (new_pull_set,p) else  down
            where
                new_pull_set = IntSet.insert p running_pulls
                winning_lines = map (isWin new_pull_set) board_list
                line_sizes = map IntSet.size winning_lines
                down = final_pull_list ps board_list new_pull_set      

        



getIntVals :: FilePath -> IO [Int]
getIntVals path = do 
                    contents <- readFile path
                    return (map (read::String->Int) (lines contents))

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)
string2int = r
    where 
        r:: [Char] -> Int
        r = read

rowString2list :: [Char] -> [Int]
rowString2list xs = map string2int (filter (\x->x /= "") $ splitOn " " xs)
    

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) =c

--main :: IO()
main = do 
            printf "Advent of Code 2021, Day 3:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            let pulled_numbers = head vals1
            let boards  = drop 2 vals1
            
            printf "    Part 1\n"
            
            let int_pulls = map string2int (splitOn "," pulled_numbers)
           
            
            
            let boards3 = split_boards ([]:(map rowString2list boards))
            let board_sets = map mkBoard boards3
           
            
           
         
           
         
        
            let (final_pull_set, last_pull) =  final_pull_list int_pulls board_sets IntSet.empty
          

            print last_pull
            let winning_score =  map (isWin final_pull_set) board_sets
            print winning_score
            let bb = getWinningBoard final_pull_set board_sets
            printf "\n"
            print bb
            let final_score = scoreBoard bb final_pull_set last_pull
            print final_score

