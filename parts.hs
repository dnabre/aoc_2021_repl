main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let lss =  map Text.unpack ls
    let xs = [read l:: Int | l<-lss]
    let solpart1 = "The answer to Part1: " ++ (show (countIncs xs))
    let solpart2 = "The answer to Part2: " ++ (show (countIncs (sumOfThree xs)))
    putStrLn solpart1
    putStrLn solpart2


import System.IO()



import Data.Char (digitToInt)
import Data.List (foldl')

bin2Dec :: String -> Int
bin2Dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd3 (_,_,c) =c


main :: IO()
main = do vals <- getVals "input.txt"
          print (part1 vals)
          print (part2 vals)


getVals :: FilePath -> IO [Int]
getVals path = do contents <- readFile path
                  return (map (read::String->Int) (lines contents))

part1 :: [Int] -> Int
part1 xs = sum $ zipWith (\a b -> if a < b then 1 else 0) xs (tail xs)

part2 :: [Int] -> Int
part2 xs = part1 $ zipWith3 (\a b c -> a+b+c) xs (tail xs) (drop 2 xs)