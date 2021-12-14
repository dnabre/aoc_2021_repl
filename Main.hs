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




-- Advent of Code 2021
-- Day 14
--  part 1 solution: 
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day14/aoc_14_test_1.txt"
part_2_test::[Char]
part_2_test = "day14/aoc_14_test_2.txt"
part_1_input::[Char]
part_1_input = "day14/aoc_14_part_1.txt"
part_2_input::[Char]
part_2_input = "day14/aoc_14_part_2.txt"




--counter x = foldr (uncurry $ insertWith (+)) empty $ zip x (repeat 1)

--common = maximum . map (\x -> (length x, head x)) . Data.List.group . sort
--rare = minimum . map (\x -> (length x, head x)) . Data.List.group . sort

part1 x = undefined


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





    
parseEqs ls =  (a,b,t)
    where
        (ps:ff:_) = splitOn " -> " ls
        a = (head ps)
        b = (head (drop 1 ps))
        t = (head ff)

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

lastLet pair_list = [ snd $ head (reverse pair_list) ]

makePairList::[Char]->[(Char,Char)]
makePairList ls = makePairList' ls []
    where
        makePairList' [] acc = reverse acc
        makePairList' (x:[]) acc =reverse acc
        makePairList' (x:y:xs) acc = makePairList' (y:xs) ((x,y):acc)



main :: IO()
main = do 
            printf "Advent of Code 2021, Day 14:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)

           
            let (template_string,eq_strings) = splitEmptyLine vals1
            let template = head template_string
         
            
            let c_list = map parseEqs eq_strings
            let c_map = insertPairListMap c_list Map.empty
           -- print c_map
            
            let pair_list = makePairList template
        
            printf "\n    Template: %s\n" template           
            let exp1 = insertChems pair_list c_map 
            printf "    After step 1: %s\n" exp1
            

            let exp2 = insertChems (makePairList exp1) c_map
            printf "    After step 2: %s\n" exp2
           
            let exp3 = insertChems (makePairList exp2) c_map
            printf "    After step 3: %s\n" exp3

            let exp4 = insertChems (makePairList exp3) c_map
            printf "    After step 4: %s\n" exp4





            let answer1 = 0
            let answer2 = 0

          --  printf "\n   Part 1    Solution: %d \n" answer1
          --  printf "\n   Part 2    Solution: %d \n" answer2

            print "done"