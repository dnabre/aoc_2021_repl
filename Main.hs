{-# LANGUAGE MultiWayIf #-}

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
-- Day 4
--  part 1 solution: 64084
--  part 2 solution: 12833

part_1_test = "day4/aoc_04_test_1.txt"
part_2_test = "day4/aoc_04_test_2.txt"

part_1_input = "day4/aoc_04_part_1.txt"
part_2_input = "day4/aoc_04_part_2.txt"

set_sum::IntSet.IntSet->Int
set_sum iset = sum (IntSet.toList iset)

part1::[Char]->[[Char]]->Int
part1  pulled_numbers boards = final_score
    where
        int_pulls = map string2int (splitOn "," pulled_numbers)
        boards3 = split_boards ([]:(map rowString2list boards))
        board_sets = map mkBoard boards3
 
        (final_pull_set, last_pull) =  final_pull_list int_pulls board_sets IntSet.empty

        bb = getWinningBoard final_pull_set board_sets
        final_score = scoreBoard bb final_pull_set last_pull



part2 = undefined
--part2::[Char]->[[Char]]->Int
--part2  pulled_numbers boards = final_score

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
    
getAllWinningBoards plist boards = [b | b<-boards, ((isWin plist b) /= IntSet.empty)]


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
string2int::[Char]->Int               
string2int = r
    where 
        r:: [Char] -> Int
        r = read

rowString2list :: [Char] -> [Int]
rowString2list xs = map string2int (filter (\x->x /= "") $ splitOn " " xs)
    
elems_until::Int->[Int]->[Int]->[Int]    
elems_until _ [] acc = (reverse acc)
elems_until e (p:ps) acc = if (e == p) then (reverse (p:acc)) else elems_until e ps (p:acc)

find_last_winner::[Int]->[[IntSet.IntSet]]->Int
find_last_winner pull_list board_sets = run (reverse pull_list) board_sets
    where
        get_win_list pull_l boards = map (isWin (IntSet.fromList pull_l)) boards
        run (p:ps) boards = if (elem IntSet.empty (get_win_list ps boards)) then p else run ps boards

--main :: IO()
main = do 
            printf "Advent of Code 2021, Day 4:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)
            let pulled_numbers1 = head vals1
            let boards1  = drop 2 vals1
        --    let score = part1 pulled_numbers1 boards1
        --   printf "\n    Part 1\n      Solution: %d \n" score
            
            let pulled_numbers = head vals2
            let boards = drop 2 vals2
            
            let int_pulls = map string2int (splitOn "," pulled_numbers)
            let boards3 = split_boards ([]:(map rowString2list boards))
            let board_sets = map mkBoard boards3
 
            let(final_pull_set, last_pull) =  final_pull_list int_pulls board_sets IntSet.empty

            let bb = getWinningBoard final_pull_set board_sets
            let final_score = scoreBoard bb final_pull_set last_pull

            let all_pulls_set = IntSet.fromList int_pulls
            let winnable = map (isWin all_pulls_set) board_sets 

           -- print winnable
            let last_pull =  (find_last_winner int_pulls board_sets)           
            let final_pull_set = IntSet.fromList (elems_until last_pull int_pulls [])
            let except_last = getAllWinningBoards (IntSet.delete last_pull final_pull_set) board_sets
            printf "\n winning boards before    last pull: %d \n" (length except_last)
            let with_last = getAllWinningBoards final_pull_set board_sets
            printf "\n winning boards including last pull: %d \n" (length with_last)
            let target_board =head $ Set.toList (Set.difference (Set.fromList with_last) (Set.fromList except_last))
            printf "\n\n targetboard: \n"
            print target_board
            let target_score = scoreBoard target_board final_pull_set last_pull
            print target_score
          --  let last_board = IntSet.difference (IntSet.fromList except_last) (IntSet.fromList with_last)
          --  printf "\n found boards: %d \n" (IntSet.size last_board)
            print final_pull_set
            printf "\n\n        done\n"