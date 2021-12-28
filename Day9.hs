module Main where

import Text.Printf
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map



-- Advent of Code 2021
-- Day 9
--  part 1 solution: 436
--  part 2 solution: 1317792

part_1_test::[Char]
part_1_test = "day9/aoc_09_test_1.txt"
part_2_test::[Char]
part_2_test = "day9/aoc_09_test_2.txt"
part_1_input::[Char]
part_1_input = "day9/aoc_09_part_1.txt"
part_2_input::[Char]
part_2_input = "day9/aoc_09_part_2.txt"



part1:: [[Char]]->Int
part1 vals = sum (map (\x-> x + 1) low_list)
    where
        m_grid = initMapGrid vals
        key_list = Map.keys m_grid
        low_list = concatMap (\k-> getLowValue k m_grid) key_list
        

     
part2::[[Char]]->Int
part2 vals2 = product basin_sizes
    where   
        m_grid = initMapGrid vals2
        l_points = getLowPoints m_grid
        basins = map (\lp->growBasin m_grid (Set.singleton lp) empty_point_set [lp]) l_points
        basin_sizes =  take 3 $ reverse $ sort (map length basins)


getLowPoints::Map.Map Point Int->[Point]
getLowPoints m_grid = low_points
    where
        key_list = Map.keys m_grid
        low_points = filter (\p-> (getLowValue p m_grid) /= [] ) key_list

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

data Point = Point Int Int deriving (Show,Eq,Ord)

addPoint::Point->(Int,Int)->Point 
addPoint (Point x y) (dx,dy) = Point (x+dx) (y+dy)

getNeighborPoints::Point->Map.Map Point Int ->[Point]
getNeighborPoints p m_grid  = f_coords
    where
        coords = map (addPoint p) [(0,1), (0,-1) , (-1,0), (1,0)]
        f_coords = filter (\np-> Map.member np m_grid) coords

getNeighbors::Point->Map.Map Point Int->[Int]
getNeighbors p m_grid  = m_values
    where
        coords = map (addPoint p) [(0,1), (0,-1) , (-1,0), (1,0)]
        f_coords = filter (\np-> Map.member np m_grid) coords
        m_values = catMaybes $ map (\np->Map.lookup np m_grid) f_coords

getLowValue::Point -> Map.Map Point Int -> [Int]
getLowValue p m_grid = r
    where   
        n_values = getNeighbors p m_grid
        Just my_val =  Map.lookup p m_grid
        r = if (all (> my_val) n_values) then [my_val] else []

growBasin:: Map.Map Point Int -> Set.Set Point -> Set.Set Point -> [Point] -> Set.Set Point
growBasin _ basin_set _  [] = basin_set
growBasin grid basin_set visited_set (pnt:p_rest) = growBasin grid n_basin n_visited_set work_list
    where
        n_visited_set = Set.insert pnt visited_set
        neigh_points::[Point]
        neigh_points = filter (\p->Set.notMember p n_visited_set )  (getNeighborPoints pnt grid) 
        (n_basin,work_list) = if (pointToValue grid pnt) /= 9 
                                then (Set.insert pnt basin_set, p_rest++neigh_points) 
                                else (basin_set,p_rest)
        
        
empty_point_set::Set.Set Point
empty_point_set = Set.empty

pointToValue::Map.Map Point Int->Point->Int
pointToValue grid point = my_val
    where
        Just my_val = Map.lookup point grid

main::IO ()
main = do 
            printf "Advent of Code 2021, Day 9:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)

            let answer1 = part1 vals1
            printf "\n   Part 1    Solution: %d \n" answer1         

            let answer2 = part2 vals2
            printf "\n   Part 2    Solution: %d \n" answer2
          

            printf "\n             done             \n"

