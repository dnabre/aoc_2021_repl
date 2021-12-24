{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}


import Text.Printf
import Text.RawString.QQ
import Data.SBV
import Data.Either

type Data = SInt64

data Reg = X | Y | Z | W deriving (Eq, Show)

data Op = Add | Mul | Div | Mod | Eql deriving (Eq, Show)

data Ins
  = Inp Reg
  | Ins Op Reg (Either Reg Int)
  deriving (Show)



{-    
main = 
     do 
     printf "Advent of Code 2021, Day 24:\n"
     let small = smallestSolution
     let big = largestSolution
     print small
     print big
-}

parse :: String -> [Ins]
parse = map (parseLine . words) . lines
  where
    parseReg "x" = Left X
    parseReg "y" = Left Y
    parseReg "z" = Left Z
    parseReg "w" = Left W
    parseReg val = Right (read val :: Int)
    parseOp "add" = Add
    parseOp "mul" = Mul
    parseOp "div" = Div
    parseOp "mod" = Mod
    parseOp "eql" = Eql
    parseOp _ = error "invalid op"
    parseLine ["inp", reg] =
      Inp . fromLeft (error "not reg") $ parseReg reg
    parseLine [op, to, reg] =
      Ins (parseOp op) (fromLeft (error "not reg") $ parseReg to) (parseReg reg)
    parseLine _ = error "invalid instruction"

data State = State {x :: Data, y :: Data, z :: Data, w :: Data} deriving (Show)

readReg :: Reg -> State -> Data
readReg X = x
readReg Y = y
readReg Z = z
readReg W = w

writeReg :: State -> Reg -> Data -> State
writeReg state X v = state {x = v}
writeReg state Y v = state {y = v}
writeReg state Z v = state {z = v}
writeReg state W v = state {w = v}

opFunc :: Op -> Data -> Data -> Data
opFunc Add = (+)
opFunc Mul = (*)
opFunc Div = sDiv
opFunc Mod = sMod
opFunc Eql = \a b -> ite (a .== b) 1 0

run :: [Data] -> [Ins] -> State
run = go (State 0 0 0 0)
  where
    go state _ [] = state
    go state (i : inp) (Inp reg : ops) = go (writeReg state reg i) inp ops
    go state inp (Ins op to from : ops) =
      go
        ( writeReg
            state
            to
            $ opFunc
              op
              (readReg to state)
              case from of
                Left reg' -> readReg reg' state
                Right val -> fromIntegral val
        )
        inp
        ops
    go _ _ _ = undefined

toNum :: [Data] -> Data
toNum = foldl1 (\acc e -> acc * 10 + e)

isValid :: [Data] -> SBool
isValid inp =
  sAll (\n -> n .>= 1 .&& n .<= 9) inp
    .&& z (run inp (parse instructions)) .== 0



largestSolution = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isValid numbers
  maximize "value" $ toNum numbers

smallestSolution = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isValid numbers
  minimize "value" $ toNum numbers
  
instructions :: String
instructions = [r|inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 12
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 16
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 4
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 1
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 5
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 12
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 0
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -5
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 7
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x 0
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 2
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 15
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 14
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 9
mul y x
add z y|]
