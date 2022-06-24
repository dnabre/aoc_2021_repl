module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char

import Data.Maybe
import qualified Data.MultiSet as MS


-- Advent of Code 2021
-- Day 22
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


newtype Span = Span (Int, Int) deriving (Eq, Ord, Show) 

mkSpan a b | b < a = error (printf "Span is backwards, %s < %s "  (show b) (show a))
mkSpan a b = Span (a,b)

pSpan::[Char] -> Span
pSpan ln = mkSpan (string2int a) (string2int b)
    where
        parts = splitOn ".." ln
        (a,b)  = ( head parts, head (tail parts))

data Action = On | Off deriving (Eq,Ord,Show)


data Cuboid = Cuboid 
    {
        spanX :: Span,
        spanY :: Span,
        spanZ :: Span
    } deriving (Eq,Ord,Show)
    
newtype Command = Command (Action, Cuboid) deriving (Eq, Ord, Show) 


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

parseLine ln = cube
    where   
        space_split::[[Char]]
        space_split = splitOn " " ln
        s_cube_range::[[Char]]
        (s_action,s_cube_range) = (head space_split, tail space_split)
        action = case s_action of
            "on"  -> On
            "off" -> Off
            _ -> error (printf "Unknown input, expecting on/off, get %s" s_action)
        s_ranges::[[Char]]
        s_ranges = map (drop 2) (splitOn "," (head s_cube_range))
        spans = (map pSpan s_ranges)
        cube = Cuboid (spans !! 0) (spans !! 1) (spans !! 2)
        

{-
parseLine ls = (on_off,(pFrom,pTo))
    where
        ws = words ls
        on_off = if (head ws == "on")  then True else False
        raw_range = head(tail ws)
        hunks =map (drop 2) $ splitOn "," raw_range
        phunks = map (splitOn "..") hunks
        rr@[[xl,xr],[yl,yr],[zl,zr]] = map (map string2int) phunks    
        (pFrom,pTo) = (ZPoint (xl-1) (yl) (zl),ZPoint (xr+1) (yr+1) (zr+1))

outside (a, b)  = ( (a < p_min )  && (b < p_min )  || (a > p_max  ) && (b > p_max ))
-}




main :: IO()
main = do 
            printf "Advent of Code 2021, Day 22:\n"
            vals2 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals2)
            vals1 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals1)

            mapM putStrLn vals1
            let line1 = head vals1
            let r = parseLine line1
            --mapM (putStrLn . show) r 
            putStrLn (show r)
            let iss = map parseLine vals1
            putStrLn ""
            mapM putStrLn (map show iss)


            


            
            


        {-
            let answer1 = part1 template1 c_map1 10
            printf "\n   Part 1    Solution: %d \n" answer1         

            let (template2,c_map2) = parseAll vals2
            let answer2 = part2 template2 c_map2
            printf "\n   Part 2    Solution: %d \n" answer2
        -}

            printf "\n\n        done\n\n"
