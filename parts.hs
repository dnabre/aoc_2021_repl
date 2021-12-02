main = do
    ls <- fmap Text.lines (Text.readFile "input")
    let lss =  map Text.unpack ls
    let xs = [read l:: Int | l<-lss]
    let solpart1 = "The answer to Part1: " ++ (show (countIncs xs))
    let solpart2 = "The answer to Part2: " ++ (show (countIncs (sumOfThree xs)))
    putStrLn solpart1
    putStrLn solpart2

    