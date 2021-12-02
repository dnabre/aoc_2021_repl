{-# LANGUAGE MultiWayIf #-}

module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split

-- Advent of Code 2021
-- Day 2
--  part 1 solution: 1990000
--  part 2 solution: 

part_1_test = "day2/aoc_02_test_1.txt"
part_2_test = "day2/aoc_02_test_2.txt"

part_1_input = "day2/aoc_02_part_1.txt"
part_2_input = "day2/aoc_02_part_2.txt"

data SubInstruction =
    Forward Int |
    Down Int|
    Up Int |
    Stop   deriving (Show) 

instructionStringFoward = "forward"
instructionStringDown = "down"
instructionStringUp = "up"


parseInstruction :: ([Char],Int) -> SubInstruction
parseInstruction (ins, num) = if    | ins == instructionStringFoward -> (Forward num)
                                    | ins == instructionStringDown -> (Down num)
                                    | ins == instructionStringUp -> (Up num)


part1 :: [SubInstruction] -> (Int,Int)
part1 (x:xs) = executeIns x (0,0) xs
    
executeIns :: SubInstruction -> (Int,Int) -> [SubInstruction] -> (Int, Int)
executeIns (Forward n) (x,y) (z:zs) = executeIns z (x+n,y) zs
executeIns (Down n) (x,y) (z:zs) = executeIns z (x,y+n) zs
executeIns (Up n) (x,y) (z:zs) = executeIns z (x,y-n) zs
executeIns (Stop) end _ = end         


part2 :: [String] -> Int
part2 _ = 0


getIntVals :: FilePath -> IO [Int]
getIntVals path = do 
                    contents <- readFile path
                    return (map (read::String->Int) (lines contents))

getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)

instructParts :: [Char] -> ([Char], Int)
instructParts s = instruct_format (splitOn " " s)
    where
        instruct_format (x:y:_) = (x,(read::String->Int) y)




main :: IO()
main = do 
            vals1 <- getStringVals part_1_input
            vals2 <- getStringVals part_2_input
            let program =  (map parseInstruction ( map instructParts vals1)) ++ [Stop]
            let final_state_1 = (part1 program)
            let answer_part_1 = (\(x,y) -> x*y) final_state_1
            --print (answer_part_1)
            printf "Part 1 \n     final location (%d,%d) -> %d\n" (fst final_state_1) (snd final_state_1) answer_part_1
            exitWith ExitSuccess
            print ( vals2)
            print (part2 vals2)