module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet


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

getManyRare::[Char]->((Char,Int),(Char,Int))
getManyRare ss = (head ml, head (reverse ml))
    where
        ml = sortOn snd $ MultiSet.toOccurList ms
        ms = MultiSet.fromList ss

part1:: [[Char]]->Int
part1 vals = sum (map (\x-> x + 1) low_list)
    where
        m_grid = initMapGrid vals
        key_list = Map.keys m_grid
        low_list = concatMap (\k-> getLowValue k m_grid) key_list
        

     

part2 x = undefined    

getLowPoints::Map.Map Point Int->[Point]
getLowPoints m_grid = low_points
    where
        key_list = Map.keys m_grid
        low_points = filter (\p-> (getLowValue p m_grid) /= [] ) key_list

initMapGrid :: [[Char]] -> Map.Map Point Int
initMapGrid vals = Map.fromList (foldl (++) [] pgp)
    where
        pps = map (\l->map digitToInt l) vals
        x_w = length (head vals)
        y_w = length vals
        g_p = gPoints x_w y_w
        gpg = zip g_p pps
        pgp = map (\(p,v)-> zip p v) gpg


         
gPoints:: Int->Int->[[Point]]
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


printPoints::Map.Map Point Int -> Int -> Int -> IO ()
printPoints g_map x_w y_w = putStr (concatMap (\s->s ++ ['\n']) lls)
    where 
        pl = Map.toList g_map
        l1 is_y =concatMap show $ map (\(Point _ _,v)-> v) $  (filter (\(Point x y,v)-> (y == is_y)) pl)
        lls =  map (\y->l1 y) [1..y_w]

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



--       let b_set2@(n_basin2,n_visited_set2,n_work_list2) = growBasin m_grid n_basin  n_visited_set n_work_list
   
--growBasin::Point->Map.Map Point Int->Set.Set Point->Set.Set Point->[Point]->Set.Set Point
growBasin grid basin_set visited_set  [] = basin_set
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

main = do 
            printf "Advent of Code 2021, Day 9:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)

            --let answer1 = part1 vals1
            --printf "\n   Part 1    Solution: %d \n" answer1         

            let m_grid = initMapGrid vals2
            let l_points = getLowPoints m_grid
            
            let x_width = length (head vals2)
            let y_width = length vals2
            let tot = x_width * y_width

            --print l_points
            let bpnt = head l_points

            let nine_count = length $ filter (\p->(pointToValue m_grid p) == 9) (Map.keys m_grid)
            printf "\n Of %d total grid points, %d are nines leaving %d\n" tot nine_count (tot-nine_count)
--(n_basin,n_visited_set,p_rest,neigh_points)
            
            --let b_set0 = growBasin m_grid (Set.singleton bpnt) empty_point_set [bpnt]  
           -- print b_set0            
            let basins = map (\lp->growBasin m_grid (Set.singleton lp) empty_point_set [lp]) l_points
            let basin_sizes =  take 3 $ reverse $ sort (map length basins)
            print basin_sizes
            
            
            
            
            let answer2 = product basin_sizes
            printf "\n   Part 2    Solution: %d \n" answer2
          

            printf "\n             done             \n"

