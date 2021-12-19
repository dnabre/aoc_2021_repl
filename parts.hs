main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let lss =  map Text.unpack ls
    let xs = [read l:: Int | l<-lss]
    let solpart1 = "The answer to Part1: " ++ (show (countIncs xs))
    let solpart2 = "The answer to Part2: " ++ (show (countIncs (sumOfThree xs)))
    putStrLn solpart1
    putStrLn solpart2


printPoints::Map.Map Point Int -> Int -> Int -> IO ()
printPoints g_map x_w y_w = putStr (concatMap (\s->s ++ ['\n']) lls)
    where 
        pl = Map.toList g_map
        l1 is_y =concatMap show $ map (\(Point _ _,v)-> v) $  (filter (\(Point x y,v)-> (y == is_y)) pl)
        lls =  map (\y->l1 y) [1..y_w]


set_sum::IntSet.IntSet->Int
set_sum iset = sum (IntSet.toList iset)
-- Set colors and write some text in those colors.
sgrExample :: IO ()
sgrExample = do
    putStrLn "Default Test"
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "this is bold"
    setSGR [SetConsoleIntensity NormalIntensity]
    setSGR [SetColor Foreground Vivid Red]
    setSGR [SetColor Background Vivid Blue]
    putStr "Red-On-Blue\n"
    setSGR [Reset]
    putStr "White-On-Black\n"


import System.IO()

            putStrLn $ "\ESC[0mdefault"
            putStrLn $ "\ESC[30mblack"
            putStrLn $ "\ESC[31mred"
            putStrLn $ "\ESC[32mgreen"
            putStrLn $ "\ESC[33myellow"
            putStrLn $ "\ESC[34mblue"
            putStrLn $ "\ESC[35mmagenta"
            putStrLn $ "\ESC[36mcyan"
            putStrLn $ "\ESC[37mwhite"


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