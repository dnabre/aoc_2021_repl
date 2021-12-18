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
-- Day 9
--  part 1 solution: 
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day9/aoc_09_test_1.txt"
part_2_test::[Char]
part_2_test = "day9/aoc_09_test_2.txt"
part_1_input::[Char]
part_1_input = "day9/aoc_09_part_1.txt"
part_2_input::[Char]
part_2_input = "day9/aoc_09_part_2.txt"

getManyRare::[Char]->((Char,Int),(Char,Int))
getManyRare ss = (head ml, head (reverse ml))
    where
        ml = sortOn snd $ MultiSet.toOccurList ms
        ms = MultiSet.fromList ss

part1 x = undefined

part2 x = undefined    
-- [[Char]] -> [(Point, Int)]

initMapGrid :: [[Char]] -> Map.Map Point Int
initMapGrid vals = Map.fromList (foldl (++) [] pgp)
    where
        pps = map (\l->map digitToInt l) vals
        x_w = length (head vals)
        y_w = length vals
        g_p = gPoints x_w y_w
        gpg = zip g_p pps
        pgp = map (\(p,v)-> zip p v) gpg


--gPoints x_w y_w = [(Point x y)| y<-[1..y_w], x<-[1..x_w]]         

gPoints x_w y_w = map (bline x_w) [1..y_w]
    where
        bline num_x y_value = [(Point x y_value) | x<-[1..num_x]]


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


data Point = Point Int Int deriving (Show,Eq,Ord)


printPoints g_map x_w y_w = mapM_ putStr (map show (l1 1))
    where 
        pl = Map.toList g_map
        l1 is_y = map (\(Point _ _,v)-> v) $  (filter (\(Point x y,v)-> (y == is_y)) pl)
        --l1 is_y =  sortOn (\(Point x y,v)->x)  (filter (\(Point x y,v)-> (y == is_y)) pl)
    
        
  --        l'   = [ (if elem (Point x y) pl then "#" else blank)
  --             ++ if x == maxX then "\n" else ""
  --             | y <- [0..maxY]
  --             , x <- [0..maxX]
  --            ]
 



main = do 
            printf "Advent of Code 2021, Day 9:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)

            
        
            let x_width = length (head vals1)
            let y_width = length (vals1)
            printf "Grid is x_width: %d y_width: %d\n" x_width y_width
            print (head vals1)
            printf "\n gPoints: \n"
            
             
            let m_grid = initMapGrid vals1
            printf "\n m_grid: \n"
            --print m_grid
            printf "calling printPoints: \n"
            printPoints m_grid x_width y_width
            printf "\n --end print Points"

            --let answer1 = part1 
            --printf "\n   Part 1    Solution: %d \n" answer1         

            --let answer2 = part2 
            --printf "\n   Part 2    Solution: %d \n" answer2



            printf "\n             done             \n"