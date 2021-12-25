module Main where

import Text.Printf
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map



-- Advent of Code 2021
-- Day 25
--  part 1 solution: 429
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day25/aoc_25_test_1.txt"
part_2_test::[Char]
part_2_test = "day25/aoc_25_test_2.txt"
part_1_input::[Char]
part_1_input = "day25/aoc_25_part_1.txt"
part_2_input::[Char]
part_2_input = "day25/aoc_25_part_2.txt"




part1 x = undefined

     
part2 x = undefined






getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

data Point = Point Int Int deriving (Show,Eq,Ord)

data Cell = CDown | CRight | CEmpty deriving  (Eq, Ord)
instance Show Cell where
    show CDown = "v"
    show CRight = ">"
    show CEmpty = "."


toCell 'v' = CDown
toCell '>' = CRight
toCell '.' = CEmpty
toCell x = error $ "bad cell value: " ++ (show x)

doNStep rl 0 = rl
doNStep rl n = doNStep (doStep rl) (n-1)

doUntil block acc = if block == block' then acc else doUntil block' (acc+1)
    where   
        block' = doStep block

doStep row_list = transpose c_step
    where
        r_step = map rowStep row_list
        t_r = transpose r_step
        c_step = map colStep t_r

colStep cs  =  map shift three_parts
    where 
        three_parts = zip3 (last cs: init cs) cs (tail cs ++ [head cs])
        shift (CDown, CEmpty, _) = CDown
        shift (_ ,CDown, CEmpty) = CEmpty
        shift (_ ,mid, _) = mid
 
rowStep cs  =  map shift three_parts
    where 
        three_parts = zip3 (last cs: init cs) cs (tail cs ++ [head cs])
        shift (CRight, CEmpty, _) = CRight
        shift (_ ,CRight, CEmpty) = CEmpty
        shift (_ ,mid, _) = mid

countList [] (d,r,e) = (d,r,e)
countList ('v':xs)  (d,r,e)  = countList xs (d+1,r,e)
countList ('>':xs)  (d,r,e)  = countList xs (d,r+1,e)
countList ('.':xs)  (d,r,e)  = countList xs (d,r,e+1)
count_list (x:xs)  _  = error $ "bad element in list " ++ (show x)   

listLast (x:[]) = x
listLast (x:xs) = listLast xs 

showCG c_row_list = map (concatMap show) c_row_list 

main::IO ()
main = do 
            printf "Advent of Code 2021, Day 25:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            
            let c_row_list = map ( map toCell ) vals1
          
            let answer1 = doUntil c_row_list 1
            print answer1 

             --print vals1
            {-
           
            let s_c_row = showCG c_row_list
            let c_row = head c_row_list
            let step1 = doNStep c_row_list 4
            printf "-------------\n"        
            mapM_ printf  (map (\l-> l ++ "\n") (showCG c_row_list))
            printf "-------------\n\n"
            mapM_ printf  (map (\l-> l ++ "\n") (showCG step1)) 
            printf "-------------\n"
            -}
{-

            let answer1 = part1 vals1
            printf "\n   Part 1    Solution: %d \n" answer1         
            let answer2 = part2 vals2
            printf "\n   Part 2    Solution: %d \n" answer2
-}        

            printf "\n             done             \n"

