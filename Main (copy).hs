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
-- Day 5
--  part 1 solution: 6548
--  part 2 solution: 19663

part_1_test = "day5/aoc_05_test_1.txt"
part_2_test = "day5/aoc_05_test_2.txt"

part_1_input = "day5/aoc_05_part_1.txt"
part_2_input = "day5/aoc_05_part_2.txt"

data Line=Line (Int,Int) (Int, Int)|LineError deriving (Show,Eq,Ord)
data Point = Point Int Int deriving (Show,Eq,Ord)

part1::[[Char]] -> Int
part1 vals1 = do
                let lls = map (splitOn "->") vals1
                let llp = map (\x->map (splitOn ",") x) lls
                let int_lines = map parseLineInt llp
                let line_list = map parsePairs int_lines 
                let h_lines = (filter horzLines line_list)
                let v_lines = (filter vertLines line_list)
                let h_points = concat $ map getHortPoints h_lines
                let v_points = concat $ map getVertPoints v_lines
                let num_points = (length h_points) + (length v_points)
                let h_map = addPoints h_points Map.empty            
                let v_map = addPoints v_points h_map
                (countPoints v_map)
            
part2::[[Char]] -> Int
part2 vals2 = do
                let lls = map (splitOn "->") vals2
                let llp = map (\x->map (splitOn ",") x) lls
                let int_lines = map parseLineInt llp
                let line_list = map parsePairs int_lines 
                let h_lines = (filter horzLines line_list)
                let v_lines = (filter vertLines line_list)
                let d_lines = (filter diagLines line_list)
                let h_points = concat $ map getHortPoints h_lines
                let v_points = concat $ map getVertPoints v_lines
                let d_points = concat $ map getDiagPoints d_lines
                let h_map = addPoints h_points Map.empty            
                let v_map = addPoints v_points h_map
                let d_map = addPoints d_points v_map
                let p_c = countPoints d_map
                p_c
           
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
    
parseLineInt ps = map (\x->map string2int x) ps

parsePairs:: [[Int]]->Line
parsePairs (p1:p2:rs) = parseLine (p1,p2)
parsePairs _ = LineError   

parseLine::([Int],[Int])->Line
parseLine ( (x1:y1:r1), (x2:y2:r2) ) = if (x1 <= x2)  then Line (x1,y1) (x2, y2) else Line (x2,y2) (x1,y1)
parseLine _ = LineError

getHortPoints::Line->[Point]
getHortPoints (Line (x1, y1) (x2, y2)) = [Point x y1 | x<-[x1 .. x2]] 

getVertPoints::Line->[Point]
getVertPoints (Line (x1, y1) (x2, y2)) = if ( y1 <= y2 ) then [Point x1 y | y<-[y1..y2]] else [Point x1 y | y<-[y2..y1]]

getDiagPoints::Line->[Point]
getDiagPoints (Line (x1,y1) (x2,y2))  = if (y1<y2) then up else down
    where
        up = [Point (x1+x) (y1+x) | x<-[0..(x2-x1)]  , (y1+x) <= y2]
        down = [Point (x1+x) (y1-x) |x<-[0..(x2-x1)] , (y1-x) >= y2]

horzLines::Line -> Bool
horzLines (Line (x1, y1) (x2, y2)) = (y1 == y2)
horzLines (LineError) = False

vertLines::Line -> Bool
vertLines (Line (x1, y1) (x2, y2)) = (x1 == x2)
vertLines (LineError) = False

diagLines::Line -> Bool
diagLines (Line (x1, y1) (x2, y2)) = (x1 /= x2) && (y1 /= y2)
diagLines (LineError) = False

addPoints::[Point]->(Map.Map Point Int)->(Map.Map Point Int)
addPoints [] m = m
addPoints (p:ps) m = if Map.member p m then addPoints ps (Map.adjust (\x->x+1) p m) else addPoints ps (Map.insert p 1 m)


countPoints::Map.Map Point Int -> Int
countPoints m = Map.foldr f 0 m
    where
        f a b = if ( a > 1) then b+1 else b
         
main :: IO()
main = do 
            printf "Advent of Code 2021, Day 5:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)
            let answer1 = part1 vals1
            printf "\n    Part 1\n        Solution: %d\n" answer1
            let answer2 = part2 vals2
            printf "\n    Part 2\n        Solution: %d\n" answer2
