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
import qualified Data.RangeSet.List as RSet

-- Advent of Code 2021
-- Day 2
--  part 1 solution: 
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day22/aoc_22_test_1.txt"
part_2_test::[Char]
part_2_test = "day22/aoc_22_test_2.txt"
part_1_input::[Char]
part_1_input = "day22/aoc_22_part_1.txt"
part_2_input::[Char]
part_2_input = "day22/aoc_22_part_2.txt"

data ZPoint = ZPoint Int Int Int deriving (Show,Eq,Ord,Bounded)

zPointToInt (ZPoint x y z) = x + (y * vMax) + (z * vMax * vMax)

intToZPoint i = ZPoint x y z
    where
        x = mod i vMax
        y = mod ( div i  vMax )  vMax
        z = i `div` ( vMax * vMax )

instance  Enum ZPoint where  
    succ zp = intToZPoint ((zPointToInt zp) + 1 )
    pred zp = intToZPoint ((zPointToInt zp) -1)
    toEnum  = intToZPoint
    fromEnum  = zPointToInt
    


vMax::Int
vMax = 1000000
vMin::Int
vMin = 1000000


(x_min,x_max) = (-50,50)
(y_min,y_max) = (-50,50)
(z_min,z_max) = (-50,50)

(p_min,p_max) = (ZPoint x_min y_min z_min, ZPoint x_max y_max z_max)


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

parseLine ls = (on_off,(pFrom,pTo))
    where
        ws = words ls
        on_off = if (head ws == "on")  then True else False
        raw_range = head(tail ws)
        hunks =map (drop 2) $ splitOn "," raw_range
        phunks = map (splitOn "..") hunks
        rr@[[xl,xr],[yl,yr],[zl,zr]] = map (map string2int) phunks    
        (pFrom,pTo) = (ZPoint xl yl zl,ZPoint xr yr zr)

outside (a, b)  = ( (a < p_min)  && (b < p_min)  || (a > p_max ) && (b > p_max))


clamp ((ZPoint xl yl zl), (ZPoint xr yr zr)) = (left,right)
    where
        left = ZPoint (max x_min xl) (max y_min yl) (max z_min zl )
        right = ZPoint (min x_max xr) (min y_max yr) (min z_max zr )

pointRange  (ZPoint xl yl zl) (ZPoint xr yr zr) = [ZPoint x y z |x<-[xl..xr],y<-[yl..yr],z<-[zl..zr]]       

process [] on_set = on_set
process (x:xs) on_set = process xs next_on
    where
        (turn_on,(leftp,rightp)) = x
        next_on = if turn_on
                    then RSet.insertRange (leftp,rightp) on_set
                    else RSet.deleteRange (leftp,rightp) on_set


nn pl = concatMap (\(ZPoint x y z)-> [x,y,z]) f_pair
    where      
        r_pair = map snd pl
        f_pair = concatMap (\(a,b)->[a,b]) r_pair
      

main :: IO()
main = do 
            printf "Advent of Code 2021, Day 22:\n"
            vals1 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals2)

            let pl = map parseLine vals1
            let rs = filter (not . outside ) $ map snd pl
            let ls = zip (map fst pl) rs
            

            let result = process ls RSet.empty
            print result
            print (RSet.size result)
            


            
            


        {-
            let answer1 = part1 template1 c_map1 10
            printf "\n   Part 1    Solution: %d \n" answer1         

            let (template2,c_map2) = parseAll vals2
            let answer2 = part2 template2 c_map2
            printf "\n   Part 2    Solution: %d \n" answer2
        -}

            printf "\n\n        done\n\n"
