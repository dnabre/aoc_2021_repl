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

data Reg = X | Y | Z | W deriving (Show, Eq)
data Op = Add | Mul | Div | Mod | Eql deriving (Show, Eq)

data Ins
  = Inp Reg
  | Ins Op Reg (Either Reg Data)
  deriving (Show, Eq)




setState::Reg->Data->(Data,Data,Data,Data)->(Data,Data,Data,Data)
setState W n (w,x,y,z) = (n,x,y,z)
setState X n (w,x,y,z) = (w,n,y,z)
setState Y n (w,x,y,z) = (w,x,n,z)
setState Z n (w,x,y,z) = (w,x,y,n)


getState::Reg->(Data,Data,Data,Data)->Data
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


parseVar::[Char]->Reg
parseVar "w" = W
parseVar "x" = X
parseVar "y" = Y
parseVar "z" = Z

parseParam::[Char]->Either Reg Data
parseParam ps = if isDigit (head ps) then Right (string2int ps)  else Left (parseVar ps)
    where
        parseVar' "w" = Left W
        parseVar' "x" = Left X
        parseVar' "y" = Left Y
        parseVar' "z" = Left Z

parseLine::[[Char]]->Ins
parseLine ["inp", var] = Inp (parseVar var)
parseLine ss = i
    where        
        [iss,vss,pss] =  ss
        vs = parseVar vss
        ps = parseParam pss
        i = case iss of 
                "add" -> Ins Add vs ps
                "mul" -> Ins Mul vs ps
                "div" -> Ins Div vs ps
                "mod" -> Ins Mod vs ps
                "eql" -> Ins Eql vs ps

doOp::Op->Data->Data->Data
doOp Add r p = r+p
doOp Mul r p = r*p
doOp Div r p = if ( p==0) then error "divide by zero" else (div r p)
doOp Mod r p = mod r p
doOp Eql r p = if (r == p) then 1 else 0

getParam::(Either Reg Data)->(Data,Data,Data,Data)->Data
getParam (Right x)  state = x
getParam (Left r)   state = getState r state

initial_state::(Data,Data,Data,Data)
initial_state = (-1,-1,-1,-1)


{-
run [] state _ = state
run ins_list@(i:is) state input@(inp:ins) = run is nextState nextInput
    where  
        (rreg,pp) = case i of
            Inp rx -> (rx,Right 0)
            Ins _ rx p -> (rx,p)
        rv = getState rreg state
        p = getParam pp state 
        oop = case i of 
            Inp _ -> (setState rreg inp, ins)
            Ins o _ _ -> (setState rreg (doOp o rv p), input) 
        (nextState,nextInput) =  oop
-}
main :: IO()
main = do 
            printf "Advent of Code 2021, Day 24:\n"
            vals1 <- getStringVals part_1_test
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)
            print vals1
            let ins = map parseLine (map words vals1)
            print ins
            let input = [1,3]
  --          let result = run ins initial_state input

{-      
            let answer1 = vals1
            printf "\n   Part 1    Solution: %d \n" answer1         

          
            let answer2 = part2 vals2
            printf "\n   Part 2    Solution: %d \n" answer2
-}
            printf "\n\n done\n\n"

