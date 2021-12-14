module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet

-- Advent of Code 2021
-- Day 14
--  part 1 solution: 2657
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day14/aoc_14_test_1.txt"
part_2_test::[Char]
part_2_test = "day14/aoc_14_test_2.txt"
part_1_input::[Char]
part_1_input = "day14/aoc_14_part_1.txt"
part_2_input::[Char]
part_2_input = "day14/aoc_14_part_2.txt"

getManyRare::[Char]->((Char,Int),(Char,Int))
getManyRare ss = (head ml, head (reverse ml))
    where
        ml = sortOn snd $ MultiSet.toOccurList ms
        ms = MultiSet.fromList ss



getMR ss = ml
    where
        ml = sort $ MultiSet.toOccurList ms
        ms = MultiSet.fromList ss


part1 template c_map  n = (o_max - o_min,length exp0)
    where
        exp0 = stepExpand template c_map n
        mr@((_,o_min),(_,o_max)) =  getManyRare exp0


part2 x = undefined
           
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

splitEmptyLine::[[Char]]->([[Char]],[[Char]])
splitEmptyLine ls = splitEmptyLine' ls []
    where
        splitEmptyLine' ("":xs) p =  (reverse p,xs)
        splitEmptyLine' (x:xs) p = splitEmptyLine' xs (x:p)





parseEqs::[Char] -> (Char, Char, Char)
parseEqs ls =  (a,b,t)
    where
        (ps:ff:_) = splitOn " -> " ls
        a = (head ps)
        b = (head (drop 1 ps))
        t = (head ff)

insertPairListMap::[(Char,Char,Char)] -> Map.Map (Char,Char) Char -> Map.Map (Char,Char) Char
insertPairListMap [] m = m
insertPairListMap ((a,b,t):xs) m = insertPairListMap xs (Map.insert (a,b) t m)


--expandPairs (a,b) m = (a:(getM mb):b:[]) 
expandPairs:: (Char,Char) -> Map.Map (Char,Char) Char -> [Char]
expandPairs (a,b) m = [a,(getM mb)]
    where
        mb = Map.lookup (a,b) m
        getM Nothing = ' '
        getM (Just t) = t

--let exp_1 = concatMap (\p-> expandPairs p c_map) pair_list

insertChems::[(Char,Char)]->Map.Map (Char, Char) Char -> [Char]
insertChems pair_list c_map = (concatMap (\p->expandPairs p c_map) pair_list) ++ (lastLet pair_list)

lastLet::[(Char,Char)]->[Char]
lastLet pair_list = [ snd $ head (reverse pair_list) ]

makePairList::[Char]->[(Char,Char)]
makePairList ls = makePairList' ls []
    where
        makePairList' [] acc = reverse acc
        makePairList' (x:[]) acc =reverse acc
        makePairList' (x:y:xs) acc = makePairList' (y:xs) ((x,y):acc)


stepExpand::[Char]-> Map.Map (Char,Char) Char -> Int -> [Char]
stepExpand p_list _ 0 = p_list
stepExpand p_list c_map n = stepExpand (insertChems (makePairList p_list) c_map) c_map (n-1)


parseAll vals1 = (template, c_map)
    where
        (template_string,eq_strings) = splitEmptyLine vals1
        template = head template_string
        c_list = map parseEqs eq_strings
        c_map = insertPairListMap c_list Map.empty

main :: IO()
main = do 
            printf "Advent of Code 2021, Day 14:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)

            let (template, c_map) = parseAll vals1
            
            let (t2,c_map2) = parseAll vals2

            let r = part1 t2 c_map 40
            print r
       
            
  

        


--stepExpand::[Char]-> Map.Map (Char,Char) Char -> Int -> [Char]
--stepExpand p_list _ 0 = p_list
--stepExpand p_list c_map n = stepExpand (insertChems (makePairList p_list) c_map) c_map (n-1)

 --           let answer1 = part1 template c_map 10
            let answer2 = 0

 --           printf "\n   Part 1    Solution: %d \n" answer1
          --  printf "\n   Part 2    Solution: %d \n" answer2

            print "done"