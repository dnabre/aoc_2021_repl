{-# LANGUAGE TypeApplications #-}

module Main where
import Text.Printf
import Data.Array
import Data.Massiv.Array
import Data.Massiv.Array.Stencil

--import Data.List.Split
--import Data.Array.Repa                          as R
--import Data.Array.Repa.Algorithms.Convolve      as C
--import Prelude as P


-- Advent of Code 2021
-- Day 17
--  part 1 solution: 19503
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day20/aoc_20_test_1.txt"
part_2_test::[Char]
part_2_test = "day20/aoc_20_test_2.txt"
part_1_input::[Char]
part_1_input = "day20/aoc_20_part_1.txt"
part_2_input::[Char]
part_2_input = "day20/aoc_20_part_2.txt"


part1 x = undefined
part2 x = undefined    
        

--over9 :: Array U Ix1 Bool -> Stencil Ix2 Bool Bool
--over9 a = makeStencil (Sz2 3 3) (0 :. 0) (\get -> a ! toInt [get (i :. j) | i <- [-1 .. 1], j <- [-1 .. 1]])
--    where toInt = foldl (\x y -> 2 * x + if y then 1 else 0) 0

--computeStencil :: Bool -> Array U Ix1 Bool -> Array U Ix2 Bool -> Array U Ix2 Bool
--computeStencil def flags = compute @U . dropWindow . applyStencil padding (over9 flags)
--    where padding = Padding (Sz2 1 1) (Sz2 3 3) (Fill def)

--solve :: Int -> Int
--solve i = length $ filter id $ toList $ iterate (computeStencil True inp . computeStencil False inp) gr  !! (i `div`2)







main :: IO()
main = do 
            printf "Advent of Code 2021, Day 20:\n"
           
  --          printf "    read %d lines of input\n" (length vals1)
       
   --         printf "    read %d lines of input\n" (length vals2)

            (a : _ : xs) <- lines <$> readFile part_1_test
            let width = length ( head xs)
            let height = length xs

            print a















            --let answer1 = 0
            --printf "\n   Part 1    Solution: %d \n" answer1         
            
            --let answer2 = part2 
            --printf "\n   Part 2    Solution: %d \n" answer2

            printf "\n---                 done                 ---\n"
