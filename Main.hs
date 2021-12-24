module Main where

import Text.Printf
import Data.List.Split
import Data.List
import Data.Char



-- Advent of Code 2021
-- Day 24
--  part 1 solution: 
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day24/aoc_24_test_1.txt"
part_2_test::[Char]
part_2_test = "day24/aoc_24_test_2.txt"
part_1_input::[Char]
part_1_input = "day24/aoc_24_part_1.txt"
part_2_input::[Char]
part_2_input = "day24/aoc_24_part_2.txt"

type Data = Int

data Param = Var | D Data deriving (Show,Eq)  
data Instruction =  Inp Var | Add Var Param | Mul Var Param | Div Var Param 
    |Mod Var Param |Eql Var Param deriving (Show,Eq)





setState W n (w,x,y,z) = (n,x,y,z)
setState X n (w,x,y,z) = (w,n,y,z)
setState Y n (w,x,y,z) = (w,x,n,z)
setState Z n (w,x,y,z) = (w,x,y,n)


getState W (w,x,y,z) = w 
getState X (w,x,y,z) = x
getState Y (w,x,y,z) = y
getState Z (w,x,y,z) = z


part1 x = undefined
part2 x = undefined


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

getStringVals::FilePath -> IO [[Char]] 
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)


parseVar::[Char]->Var
parseVar "w" = W
parseVar "x" = X
parseVar "y" = Y
parseVar "z" = Z

parseParam::[Char]->Param
parseParam ps = if isDigit (head ps) then D (string2int ps)  else parseVar ps 

parseLine::[Char]->Instruction
parseLine ss = if (head ss) == 'i' then parseLine (ss ++ "0") else i 
    where        
        [iss,vss,pss] = words ss
        vs = parseVar vss
        ps = parseParam pss
        i = case iss of 
                "inp" -> Inp vs
                "add" -> Add vs ps
                "mul" -> Mul vs ps
                "div" -> Div vs ps
                "mod" -> Mod vs ps
                "eql" -> Eql vs ps


main :: IO()
main = do 
            printf "Advent of Code 2021, Day 24:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            --print vals1
            let ins = map parseLine vals1
            print ins
{-      
            let answer1 = vals1
            printf "\n   Part 1    Solution: %d \n" answer1         

          
            let answer2 = part2 vals2
            printf "\n   Part 2    Solution: %d \n" answer2
-}
            printf "\n\n done\n\n"

