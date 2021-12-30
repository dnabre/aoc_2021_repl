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
--  part 1 solution: 1755
--  part 2 solution: 212

part_1_test::[Char]
part_1_test = "day11/aoc_11_test_1.txt"
part_2_test::[Char]
part_2_test = "day11/aoc_11_test_2.txt"

part_1_input::[Char]
part_1_input = "day11/aoc_11_part_1.txt"
part_2_input::[Char]
part_2_input = "day11/aoc_11_part_2.txt"

data Point = Point Int Int deriving (Show,Eq,Ord)

part1 vals =   r
    where
        (step100,r) =  steps m_grid 100
        m_grid = initMapGrid vals

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

digitToChar::Int->Char
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

neighbors::Point->[Point]
neighbors (Point x y) = [Point (x+x') (y+y') | x'<-[-1..1], y'<-[-1..1]]
        

        
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


flash = Map.keys .  Map.filter (> 9)

incrementGrid::Map.Map Point Int->Map.Map Point Int
incrementGrid = Map.map (\e->e+1)



incrementJust::Map.Map Point Int->[Point]->Int->Map.Map Point Int
incrementJust m_map [] n = m_map
incrementJust m_map (x:xs) n = incrementJust (Map.adjust (\x->x+n) x m_map) xs n


doFlash::Map.Map Point Int -> (Map.Map Point Int, Int)
doFlash m_map = length <$> doFlash' (m_map,[])

doFlash'::(Map.Map Point Int, [Point])->(Map.Map Point Int, [Point])
doFlash' (p_map, flashed_already) = if (length flashing) == 0 then (n_map,flashed_already) else doFlash' (n_map ,(flashed_already ++ flashing))
    where
        flashing = flash p_map
        all_next_toflash  = concatMap neighbors flashing 
        flashed = foldr (Map.adjust (+1)) p_map all_next_toflash
        n_map = foldr (Map.adjust (const 0)) flashed (flashed_already ++ flashing)

      



step  = doFlash . incrementGrid


steps :: Map.Map Point Int -> Int -> (Map.Map Point Int, Int)
steps el n = foldr sumStep (el, 0) [1..n]
    where
        sumStep :: Int -> (Map.Map Point Int, Int) -> (Map.Map Point Int, Int)
        sumStep _ (el, acc) = (+) acc <$> step el


findStepsToSync m_map width height = (a,b)
    where
        allFlashed (_, (_, flashes)) = flashes == (width * height)
        enumSteps (steps, (m_map, _)) = (steps +1, step m_map)
        (a,b) = until allFlashed enumSteps (0, (m_map, 0))


main = do 
            printf "Advent of Code 2021, Day 10:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals2)

            printf "\n vals1 grid, width %d height %d\n" (length (head vals1)) (length vals1)
            printf "vals2 grid, width %d height %d\n" (length (head vals2)) (length vals2)
            
            let answer1 = part1 vals1
            print answer1

            let m_grid = initMapGrid vals2
         
            printf "\n"

            let z@(a,b) = findStepsToSync m_grid (length (head vals2)) (length vals2)
            print a
            printf "\n\n"
            print b
            printf "\n"

            printf "\n\n    done \n \n"





      
        