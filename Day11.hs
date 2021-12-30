

import Text.Printf
import Data.Char
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

part1::[[Char]]->Int
part1 vals =   r
    where
        (_,r) =  steps m_grid 100
        m_grid = initMapGrid vals

part2::[[Char]]->Int
part2 vals2 = a
    where
        (a,_) = findStepsToSync m_grid (length (head vals2)) (length vals2)
        m_grid = initMapGrid vals2

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


getStringVals::FilePath -> IO [[Char]] 
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)


neighbors::Point->[Point]
neighbors (Point x y) = [Point (x+x') (y+y') | x'<-[-1..1], y'<-[-1..1]]
        

flash::Map.Map Point Int -> [Point]
flash m = Map.keys ( Map.filter (> 9) m)

incrementGrid::Map.Map Point Int->Map.Map Point Int
incrementGrid = Map.map (\e->e+1)

doFlash::Map.Map Point Int -> (Map.Map Point Int, Int)
doFlash m_map = length <$> doFlash' (m_map,[])

doFlash'::(Map.Map Point Int, [Point])->(Map.Map Point Int, [Point])
doFlash' (p_map, flashed_already) = if (length flashing) == 0 
                                        then (n_map,flashed_already) 
                                        else doFlash' (n_map ,(flashed_already ++ flashing))
    where
        flashing = flash p_map
        all_next_toflash  = concatMap neighbors flashing 
        flashed = foldr (Map.adjust (+1)) p_map all_next_toflash
        n_map = foldr (Map.adjust (const 0)) flashed (flashed_already ++ flashing)

      


step::Map.Map Point Int->(Map.Map Point Int, Int)
step  = doFlash . incrementGrid


steps :: Map.Map Point Int -> Int -> (Map.Map Point Int, Int)
steps el n = foldr sumStep (el, 0) [1..n]
    where
        sumStep :: Int -> (Map.Map Point Int, Int) -> (Map.Map Point Int, Int)
        sumStep _ (tel, acc) = (+) acc <$> step tel

findStepsToSync::Map.Map Point Int -> Int -> Int -> (Int, (Map.Map Point Int, Int))
findStepsToSync m_map width height = (a,b)
    where
        allFlashed (_, (_, flashes)) = flashes == (width * height)
        enumSteps (steps', (n_map, _)) = (steps' +1, step n_map)
        (a,b) = until allFlashed enumSteps (0, (m_map, 0))

main::IO ()
main = do 
            printf "Advent of Code 2021, Day 10:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals2)
        
         
         
            let answer1 = part1 vals1
            printf "\n   Part 1    Solution: %d \n" answer1         

            let answer2 = part2 vals2
            printf "\n   Part 2    Solution: %d \n" answer2

            printf "\n\n    done \n \n"





      
        
