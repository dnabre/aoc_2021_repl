module Main where

import Text.Printf
import Data.List.Split
import Data.List
import Data.Char



-- Advent of Code 2021
-- Day 24
--  part 1 solution: 96929994293996
--  part 2 solution: 41811761181141

-- TODO: Clean up code


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

getString = do
                contents <- readFile "day24/aoc_24_part_1.txt"
                return contents

parseVar::[Char]->Reg
parseVar "w" = W
parseVar "x" = X
parseVar "y" = Y
parseVar "z" = Z
parseVar x = error $ "error parsing register: " ++ (show x)

parseParam::[Char]->Either Reg Data
parseParam ps = if start_is_number then Right (string2int ps)  else Left (parseVar ps)
    where
        start_is_number = (isDigit (head ps)) || ((head ps) == '-')
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
doOp Mod r p = if (p<0) then error $"mod by negative, mod ("++(show r) ++" " ++(show p) ++ ")" else  mod r p
doOp Eql r p = if (r == p) then 1 else 0

getParam::(Either Reg Data)->(Data,Data,Data,Data)->Data
getParam (Right x)  state = x
getParam (Left r)   state = getState r state

initial_state::(Data,Data,Data,Data)
initial_state = (0,0,0,0)

go::[Ins]->[Data]->(Data,Data,Data,Data)->(Data,Data,Data,Data)
go [] input state = if input == [] then state else error $ "leftover input: " ++ (show input)
go ((Inp rn):codes) (input0:input_rest) state = go codes input_rest (setState rn input0 state)
go ((Inp rn):cs) [] s= error $ "no input to read state: " ++ (show s) ++ " code: " ++ (show cs)
go ((Ins op rn epr):codes) input state = go codes input nextState
    where 
        reg_value::Data
        reg_value = getState rn state
        param_value::Data
        param_value = case epr of
                        Left prn -> getState prn state
                        Right constant -> constant 
        result::Data
        result = doOp op reg_value param_value
        nextState::(Data,Data,Data,Data)
        nextState = setState rn result state


toData::Int->Data
toData x = x
toDataList::[Int]->[Data]
toDataList ls = map toData ls

main :: IO()
main = do 
            printf "Advent of Code 2021, Day 24:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
         --   vals2 <- getStringVals part_2_test
         --   printf "    read %d lines of input\n" (length vals2)
          --  print vals1
            let ins = map parseLine (map words vals1)
            --print ins
            let input_string = "96929994293996"
            let input_string_min = "41811761181141"
            let input = map digitToInt input_string
            printf "input: %s \n" (show input)
            let result = go ins (toDataList input) initial_state
            printf "state: %s, w=%d x=%d y=%d z=%d , valid: %s\n" (show result) 
                                (getState W result) (getState X result) 
                                (getState Y result) (getState Z result)
                                (show ((getState Z result) == 0))
            let input1 = map digitToInt input_string_min
            printf "input: %s \n" (show input1)
            let result = go ins (toDataList input1) initial_state
            printf "state: %s, w=%d x=%d y=%d z=%d , valid: %s\n" (show result) 
                                (getState W result) (getState X result) 
                                (getState Y result) (getState Z result)
                                (show ((getState Z result) == 0))



            printf "\n\n done\n\n"

