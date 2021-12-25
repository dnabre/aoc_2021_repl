module Main where

import Text.Printf
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map



-- Advent of Code 2021
-- Day 25
--  part 1 solution:
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

addPoint::Point->(Int,Int)->Point 
addPoint (Point x y) (dx,dy) = Point (x+dx) (y+dy)

        
        
empty_point_set::Set.Set Point
empty_point_set = Set.empty

pointToValue::Map.Map Point Int->Point->Int
pointToValue grid point = my_val
    where
        Just my_val = Map.lookup point grid

wrap (Point x y) width height
    | x > width = (Point 1 y)
    | y > height = (Point x 1)

right Point x y = Point (x+1) y
down Point x y = Point x y+1

targetLocs downs rights width height = (t_downs, t_rights)
    where
        w ls = map (\p-> wrap p width height)
        t_downs = w $ map (\(Point x y)->Point x (y+1)) t_downs
        r_downs = w $ map (\(Point x y)->Point (x+1) y) r_downs
        



main::IO ()
main = do 
            printf "Advent of Code 2021, Day 25:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)

            print vals1
            let width = length (head vals1)
            let height = length vals1
            print (width,height)
            let r_coords = [Point x y | x<-[1..width],y<-[1..height]]
            let r_input = foldl (++) [] vals1
                       
            let mm = (zip r_coords r_input)
            let mm2 = filter (\(Point x y,v)-> v /= '.') mm
            let dps = filter (\(Point x y,v)-> v == 'v') mm2
            let rps = filter (\(Point x y,v)-> v == '>') mm2
            let downs =  Set.fromList $ map fst dps
            let rights = Set.fromList $ map fst rps
            print downs
            print rights
            print (length rights,length downs)


{-

            let answer1 = part1 vals1
            printf "\n   Part 1    Solution: %d \n" answer1         
            let answer2 = part2 vals2
            printf "\n   Part 2    Solution: %d \n" answer2
-}        

            printf "\n             done             \n"

