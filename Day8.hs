

import Text.Printf
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

--  Advent of Code 2021
--  Day 8
--      part 1 solution: 521
--      part 2 solution: 1016804

part_1_test::[Char]
part_1_test = "day8/aoc_08_test_1.txt"
part_2_test::[Char]
part_2_test = "day8/aoc_08_test_2.txt"
part_1_input::[Char]
part_1_input = "day8/aoc_08_part_1.txt"
part_2_input::[Char]
part_2_input = "day8/aoc_08_part_2.txt"

part1::[[Char]]->Int
part1 vals1 = length $ filter (\c->elem c to_count) outputs
    where
        to_count = "1478"
        outputs = concat (processLines vals1)

part2::[[Char]]->Int
part2 vals2 = sum int_list
    where
        num_strings = processLines vals2
        int_list = map string2int num_strings
           
data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Segment = A | B | C | D | E | F | G
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

charFromDigit::Digit->Char
charFromDigit D0 = '0'
charFromDigit D1 = '1'
charFromDigit D2 = '2'
charFromDigit D3 = '3'
charFromDigit D4 = '4'
charFromDigit D5 = '5'
charFromDigit D6 = '6'
charFromDigit D7 = '7'
charFromDigit D8 = '8'
charFromDigit D9 = '9'

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

string2int::[Char]->Int
string2int = r
    where 
        r:: [Char] -> Int
        r = read
    
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

bySize::Digit->[Set.Set Char]->[Set.Set Char]
bySize d set_list = withSize (number_segments d) set_list

withSize::Int->[Set.Set Char]->[Set.Set Char]
withSize n set_list =  filter (\s->(Set.size s) == n) set_list

aNg::Show p=> [p]->p
aNg ls 
    |  (length ls) == 1 =  head ls
    |  otherwise  =  error $"assertAndGet list is not length one " ++ (show ls)

haveSubSet::[Set.Set Char]->Set.Set Char->[Set.Set Char]
haveSubSet ls ss = filter (\s->Set.isSubsetOf ss s) ls

build_map::[Set.Set Char] -> Map.Map (Set.Set Char) Digit
build_map ls =  Map.fromList $ zip r ds
    where
        r = [zero,one,two,three,four,five,six,seven,eight,nine]
        ds = [D0,D1,D2,D3,D4,D5,D6,D7,D8,D9]
        one = aNg (bySize D1 ls)
        four = aNg (bySize D4 ls)
        seven = aNg (bySize D7 ls)
        eight = aNg (bySize D8 ls)
        ls_m1478 = ls \\ [one,four,seven,eight]
        six = aNg $bySize D6 $filter (\s->1 == Set.size (Set.intersection s one) ) ls_m1478
        ls_w02359 = delete six ls_m1478
        nine = aNg $ haveSubSet ls_w02359 four
        ls_w0235 = delete nine ls_w02359
        zero = aNg (bySize D0 ls_w0235)
        ls_w235 = delete zero ls_w0235
        three = aNg $ filter (\s-> 2 == Set.size (Set.intersection one s)) ls_w235
        ls_w25 = delete three ls_w235
        five = aNg $ filter (\s-> 2 == Set.size (Set.intersection (Set.difference four one) s)) ls_w25
        two = aNg $ delete five ls_w25

decodeLine::[Char]-> [Digit]
decodeLine c_line = d_line
    where
        s_map = buildDecoder c_line
        line_set = map Set.fromList $ drop 11 (words c_line)
        d_line = map (setToDigit s_map) line_set

buildDecoder::[Char]->Map.Map (Set.Set Char) Digit        
buildDecoder vals2 = s_map
    where 
        line_one = filter (\w->w /= "|") (words vals2)
        set_list = nub $ map Set.fromList line_one
        s_map  = build_map set_list

setToDigit::Map.Map (Set.Set Char) Digit -> Set.Set Char -> Digit
setToDigit s_map ss = c
    where
        Just c = Map.lookup ss s_map 
       
processLines::[[Char]]->[[Char]]
processLines  = map (\l->map charFromDigit (decodeLine l)) 
 
main :: IO()
main = do
            printf "Advent of Code 2021, Day 8:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)
            
            let answer1 = part1 vals1
            printf "   Part 1    Solution: %d \n" answer1  
            let answer2 = part2 vals2
            printf "   Part 2    Solution: %d \n" answer2


            printf "\n   ---- done ----\n"