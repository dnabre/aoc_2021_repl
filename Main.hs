module Main where

import Text.Printf
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Advent of Code 2021
-- Day 8
--  part 1 solution: 521
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day8/aoc_08_test_1.txt"
part_2_test::[Char]
part_2_test = "day8/aoc_08_test_2.txt"
part_1_input::[Char]
part_1_input = "day8/aoc_08_part_1.txt"
part_2_input::[Char]
part_2_input = "day8/aoc_08_part_2.txt"

part1 vals1 =  length $ concat tt
    where
        slines = map parseLineToSetList1 vals1
        tt = map part1_filter slines

part2 x = undefined

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Segment = A | B | C | D | E | F | G
    deriving (Bounded, Enum, Eq, Ord, Read, Show)


digitToChar d =  snd $  head  $ filter (\(dig,c)-> dig == d)         $zip ss ds
    where
        ds = [D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]
        ss = "0123456789"


charToSegMent 'a' = A
charToSegMent 'b' = B
charToSegMent 'c' = C
charToSegMent 'd' = D
charToSegMent 'e' = E
charToSegMent 'f' = F
charToSegMent 'g' = G
charToSegMent x = error $ "unknown char, chartoSegMent " ++ [x]

segmentTochar A = 'a'
segmentTochar B = 'b'
segmentTochar C = 'c'
segmentTochar D = 'd'
segmentTochar E = 'e'
segmentTochar F = 'f'
segmentTochar G = 'g'



getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

string2int::[Char]->Int
string2int = r
    where 
        r:: [Char] -> Int
        r = read

stringToSymSet::[Char]-> Set.Set Segment
stringToSymSet w= Set.fromList (map charToSegMent w)
    
number_segments::Digit->Int 
number_segments D1 = 2
number_segments D7 = 3
number_segments D4 = 4
number_segments D8 = 7
number_segments D5 = 5
number_segments D2 = 5
number_segments D3 = 5
number_segments D0 = 6
number_segments D6 = 6
number_segments D9 = 6


segmentByNum1 seg_set =  filter (\(n,d)-> s==n ) s_d_p
    where
        s::Int
        s = Set.size seg_set 
        opts::[Digit]
        opts = [D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]
        ns::[Int]
        ns = map number_segments opts    
        s_d_p = zip ns opts

segmentByNum2 seg_set =  filter (\(_,n,d)-> s==n ) s_d_p
    where
        s::Int
        s = Set.size seg_set 
        string_list = map sort $ take (length opts) (repeat (segmentSetToString seg_set))
        opts::[Digit]
        opts = [D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]
        ns::[Int]
        ns = map number_segments opts    
        s_d_p = zip3 string_list ns opts


parseLineToSetList1 ln = map segmentByNum1 $map stringToSymSet (words $ (splitOn " | " ln) !! 1)

parseLineToSetList2 ln = map segmentByNum2 $map stringToSymSet (words $ (splitOn " | " ln) !! 1)

part1_filter st =  filter (\(_,d)-> elem d possibleDigits) flat_list
    where
        possibleDigits::[Digit]
        possibleDigits =  [D1,D4,D7,D8]
        flat_list = concat st


segmentSetToString s = map segmentTochar (Set.toList s)

main :: IO()
main = do
            printf "Advent of Code 2021, Day 8:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals2)
            -- 1,4,7,8
            --print vals1
            --let ln = head vals1 
            --let st =  map segmentByNum $map stringToSymSet (words $ (splitOn " | " ln) !! 1)
            
            let slines = map parseLineToSetList1 vals2 
            let slines2 = head $ map parseLineToSetList2 vals2
            let st = map (\ln-> (map segmentByNum2 $map stringToSymSet (words $ (splitOn " | " ln) !! 1))) vals2
            --mapM_ print st
                        
            let setToDigit = Map.empty
            let sl =  nub   $ concat slines2
            mapM_ print sl
    

            let answer1 = part1 vals1
            printf "\n   Part 1    Solution: %d \n" answer1  
            --let answer1 = part1 vals1
            --printf "\n   Part 1    Solution: %d \n" answer1  


            print "done"