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
-- Day 17
--  part 1 solution: 19503
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day17/aoc_17_test_1.txt"
part_2_test::[Char]
part_2_test = "day17/aoc_17_test_2.txt"
part_1_input::[Char]
part_1_input = "day17/aoc_17_part_1.txt"
part_2_input::[Char]
part_2_input = "day17/aoc_17_part_2.txt"

part1::(Int,Int)->(Int,Int)->Int
part1 (minX,maxX) (minY,maxY) = maxYPos
    where
        maxYVel = negate minY - 1
        maxYPos = maxYVel * (maxYVel +1) `div` 2



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


main :: IO()
main = do 
            printf "Advent of Code 2021, Day 17:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)

           
            let answer1 = part1 (57,116) (-198,-148)
            printf "\n   Part 1    Solution: %d \n" answer1         

            
            --let answer2 = part2 
            --printf "\n   Part 2    Solution: %d \n" answer2

            printf "\n---                 done                 ---\n"
{-
main = putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2

(x_min,x_max) = (57,116)
(y_min,y_max) = (-198,-148)

ys velocity = takeWhile (>= y_min) $
              scanl (+) 0 $ iterate (\y -> y - 1)        velocity
xs velocity = scanl (+) 0 $ iterate (\x -> x - signum x) velocity

xss = xs <$> [0 .. x_max]
yss = [ys' | ys' <- ys <$> [y_min .. abs y_min], last ys' <= y_max ]

inBounds (x,y) = y >= y_min && y <= y_max &&
                  x >= x_min && x <= x_max

trajectories = [ trajectory | ys <- yss, xs <- xss
                , let trajectory = zip xs ys
                , any inBounds trajectory ]

part1 = maximum . map snd $ last trajectories
part2 = length trajectories
-}
