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
-- Day 13
--  part 1 solution: 775
--  part 2 solution: REUPUPKR

part_1_test::[Char]
part_1_test = "day13/aoc_13_test_1.txt"
part_2_test::[Char]
part_2_test = "day13/aoc_13_test_2.txt"
part_1_input::[Char]
part_1_input = "day13/aoc_13_part_1.txt"
part_2_input::[Char]
part_2_input = "day13/aoc_13_part_2.txt"


data Point = Point Int Int deriving (Show,Eq,Ord)
data Fold = FoldX Int | FoldY Int deriving (Show,Eq,Ord)

part1 :: [Point] -> [Fold] -> Int
part1 ps (f:fs) = (Set.size p_set2)
    where 
        p_set = Set.fromList ps
        p_set2= foldOn f p_set

part2 :: Set.Set Point -> [Fold] -> Set.Set Point
part2 ps [] = ps
part2 ps (f:fs) = part2 (foldOn f ps) fs
           
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

splitEmptyLine::[[Char]]->[[Char]]->([[Char]],[[Char]])
splitEmptyLine ("":xs) p =  (reverse p,xs)
splitEmptyLine (x:xs) p = splitEmptyLine xs (x:p)

parseFolds::[Char] -> Fold
parseFolds s = if (f_ch == 'x')  then (FoldX n) else (FoldY n)
    where
        (fold_type:fold_amount:[]) = splitOn "=" s
        n = string2int fold_amount
        f_ch = head ( reverse fold_type)

parsePoints::[Char]->Point
parsePoints ps = Point x y
    where
        (sx:sy:_) = splitOn "," ps
        x = string2int sx
        y = string2int sy


foldOn :: Fold-> Set.Set Point -> Set.Set Point
foldOn f ps =  doFold f ps
    where
        mirror a x  | x <= a = x
                    | otherwise = 2 * a - x 
        doFold (FoldX n) = Set.map (\(Point x y)-> Point (mirror n x) y) 
        doFold (FoldY n) = Set.map (\(Point x y)-> Point x (mirror n y)) 
    

printPoints:: Set.Set Point -> [Char]  -> IO ()
printPoints pz blank = mapM_ putStr l'
  where 
        pl = Set.toList pz
        maxX = maximum [x | (Point x _) <- pl]
        maxY = maximum [y | (Point _ y) <- pl]
        l'   = [ (if elem (Point x y) pl then "#" else blank)
               ++ if x == maxX then "\n" else ""
               | y <- [0..maxY]
               , x <- [0..maxX]
               ]
    
main :: IO()
main = do 
            printf "Advent of Code 2021, Day 13:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_input
            printf "    read %d lines of input\n" (length vals2)
            
            let (p_list,f_list) = splitEmptyLine vals1 []                       
            let ps = map parsePoints p_list
            let fs = map parseFolds f_list
            let answer1 =  part1 ps fs 
         
            printf "\n   Part 1    Solution: %d \n\n" answer1

            let (p_list,f_list) = splitEmptyLine vals1 []                       
            let ps = map parsePoints p_list
            let fs = map parseFolds f_list
            let answer2 = part2 (Set.fromList ps) fs  
            
            printPoints answer2 " "
            let answer_text = "REUPUPKR"
            printf "\n   Part 2    Solution: %s \n" answer_text

