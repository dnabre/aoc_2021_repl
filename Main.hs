{-# LANGUAGE MultiWayIf #-}

module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Advent of Code 2021
-- Day 11
--  part 1 solution: 
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day11/aoc_11_test_1.txt"
part_2_test::[Char]
part_2_test = "day11/aoc_11_test_2.txt"

part_1_input::[Char]
part_1_input = "day11/aoc_11_part_1.txt"
part_2_input::[Char]
part_2_input = "day11/aoc_11_part_2.txt"

data Point = Point Int Int deriving (Show,Eq,Ord)

part1 x = undefined
part2 x = undefined


initMapGrid :: [[Char]] -> Map.Map Point Int
initMapGrid vals = Map.fromList (foldl (++) [] pgp)
    where
        pps = map (\l->map digitToInt l) vals
        x_w = length (head vals)
        y_w = length vals
        g_p = gPoints x_w y_w
        gpg = zip g_p pps
        pgp = map (\(p,v)-> zip p v) gpg


digitToChar 0 = '0'
digitToChar 1 = '1'
digitToChar 2 = '2'
digitToChar 3 = '3'
digitToChar 4 = '4'
digitToChar 5 = '5'
digitToChar 6 = '6'
digitToChar 7 = '7'
digitToChar 8 = '8'
digitToChar 9 = '9'
digitToChar _ = 'x'

         
gPoints:: Int->Int->[[Point]]
gPoints x_w y_w = map (bline x_w) [1..y_w]
    where
        bline num_x y_value = [(Point x y_value) | x<-[1..num_x]]



getIntVals :: FilePath -> IO [Int]
getIntVals path = do 
                    contents <- readFile path
                    return (map (read::String->Int) (lines contents))

getStringVals::FilePath -> IO [[Char]] 
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)



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


        
empty_point_set::Set.Set Point
empty_point_set = Set.empty

pointToValue::Map.Map Point Int->Point->Int
pointToValue grid point = my_val
    where
        Just my_val = Map.lookup point grid


printGrid::Map.Map Point Int -> IO()
printGrid m_grid = do
            let y_coords = [1..10]
            let first_line_p = [Point x y |x<-[1..10],y<-[1]]
            let m_y_lines = map (\y-> map (\(Point a b)-> Point a y) first_line_p) y_coords
            let pair_list = map (\k->Map.lookup k m_grid) first_line_p
            let v_list = map digitToChar $  catMaybes pair_list
            let ypair_list = map (\l->catMaybes $ map (\k->Map.lookup k m_grid) l) m_y_lines
            let n_lines = map (\l-> (map digitToChar l) ++ "\n" )  ypair_list
            mapM_ printf n_lines

        {-
            to do step, 
                increment all
                main two sets, alreadyFlashed and newFlash
                repeat until newFlash is empty
                then update all flashed to 0

        -}
main = do 
            printf "Advent of Code 2021, Day 10:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals2)

            let m_grid = initMapGrid vals1
         
            printf "\n"
            printGrid m_grid
            
            
            


            printf "\n\n    done \n \n"





      
        