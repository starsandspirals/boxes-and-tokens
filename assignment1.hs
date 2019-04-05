{-
     COM2001 Spring Assignment 1
     Haskell Template
     (c) 2018 Mike Stannett
     Email: m.stannett@sheffield.ac.uk
-}

-- If you would like to add your name and/or registration number
-- to this file, please do so here:
-- Daniel Marshall
-- acb16dm
-- 160152953

import Debug.Trace

type Input  = Int
type Output = Int

-- A program is something that tells a computer how to
-- move from one configuration to another, how to
-- recognize when a configuration represents a valid
-- accept state, and so on.

class (Eq cfg) => ProgrammableComputer cfg where
  initialise   :: Program -> [Input] -> cfg
  getOutput    :: cfg -> Output
  acceptState  :: Program -> cfg -> Bool
  doNextMove   :: Program -> cfg -> cfg
  runFrom      :: Program -> cfg -> cfg
  runProgram   :: Program -> [Input] -> cfg
  -- Default implementation
  runProgram p is = runFrom p (initialise p is)

-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same
--                 number of tokens, jump to instruction t
--
data Instruction
  = CLR {box :: Int}
  | INC {box :: Int}
  | JEQ {box1   :: Int,
         box2   :: Int,
         target :: Int}
  deriving (Eq, Show)

type Program = [Instruction]

-- PROBLEM 1. YOUR CODE HERE
-- --------------------------
-- Each instruction in a program refers to one or
-- more boxes.  What is the highest box number used
-- anywhere in the program?

-- The maxBoxNum function takes in a program and returns
-- the highest box number used in that program, by mapping
-- the getNum function over all the boxes and then finding
-- the maximum.
-- *Main> maxBoxNum [INC 3, CLR 2, JEQ 0 1 0]
-- 3
-- *Main> maxBoxNum [INC 4, CLR 7, JEQ 5 6 0]
-- 7
-- *Main> maxBoxNum [INC 8, CLR 9, JEQ 10 0 0]
-- 10

maxBoxNum :: Program -> Int
maxBoxNum p = maximum (map (\x -> getNum x) p)

-- The getNum function takes an instruction and returns
-- the highest box number used in that instruction; for
-- CLR and INC instructions this is simply x, whereas
-- for JEQ this is the max of x and y.
-- *Main> getNum (CLR 0)
-- 0
-- *Main> getNum (INC 1)
-- 1
-- *Main> getNum (JEQ 2 3 0)
-- 3

getNum :: Instruction -> Int
getNum (CLR x) = x
getNum (INC x) = x
getNum (JEQ x y _) = max x y

-- The configuration of a BATcomputer is given once
-- you know how many tokens are in each box, and
-- which instruction should be executed next
data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)

-- PROBLEM 2. YOUR CODE HERE
-- --------------------------

-- *Main> show (BATConfig [] 0)
-- "boxes = []; counter = 0"


instance Show BATConfig where

    show (BATConfig b c) = "boxes = " ++ show b ++ "; counter = " ++ show c

-- IMPLEMENTING THE BATComputer
-- ============================
-- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
-- Box 0 can be used by programs for calculations.

instance ProgrammableComputer BATConfig  where

    -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg

    -- The initialise function takes in a program and the user
    -- input and returns the initalised configuration, with
    -- the counter set to 0 and the boxes filled with the input,
    -- and remaining boxes set to 0.
    initialise p i = (BATConfig boxes 0)
      where
        boxes = 0:(i ++ replicate (maxBoxNum p - length i) 0)

    -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool

    -- The acceptState function takes in a program and a
    -- configuration and returns True if the computer is
    -- in an accept state, i.e. if the counter is past
    -- the end of the program.
    acceptState p (BATConfig _ c) = c >= length p

    -- PROBLEM 5: doNextMove   :: Program -> cfg -> cfg

    -- The doNextMove function takes in a program and a
    -- configuration and returns the configuration after
    -- the next move has been performed, using the
    -- nextMove function.
    doNextMove p cfg@(BATConfig _ c) = nextMove (p!!c) cfg

    -- PROBLEM 6: runFrom      :: Program -> cfg -> cfg

    -- The runFrom function takes in a program and a
    -- configuration and runs the next move until the
    -- computer is in an accept state, at which point
    -- the configuration is returned.
    runFrom p cfg
      | acceptState p cfg = cfg
      | otherwise = runFrom p (doNextMove p cfg)

    -- PROBLEM 7: getOutput    :: cfg -> Output

    -- The getOutput function takes in a configuration and
    -- returns the contents of box 1.
    getOutput (BATConfig b _) = b!!1

-- The nextMove function takes in an instruction and a configuration,
-- performs the next move and returns the new configuration. For CLR
-- and INC instructions this uses the funcBox function to perform the
-- correct operation on the box; for JEQ instructions we check if the
-- boxes are equal and if so jump the counter to instruction n, and
-- otherwise just increment the counter by 1 as normal.
nextMove :: Instruction -> BATConfig -> BATConfig
nextMove (CLR x) cfg = traceShow "CLR" funcBox (*0) x cfg
nextMove (INC x) cfg = traceShow "INC" funcBox (+1) x cfg
nextMove (JEQ x y n) (BATConfig b c)
  | b!!x == b!!y = traceShow "JEQ true" (BATConfig b n)
  | otherwise = traceShow "JEQ false" BATConfig b (c+1)

-- The funcBox function takes a function, a box number and a configuration,
-- applies the function to the specified box and then returns the new
-- configuration, with the counter incremented by 1.
funcBox :: (Int -> Int) -> Int -> BATConfig -> BATConfig
funcBox f x (BATConfig b c) = BATConfig new (c+1)
  where
    (front, box:back) = splitAt x b
    new = front ++ [f box] ++ back

-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs

-- *Main> execute [INC 1] [1,2,3]
-- 2
-- *Main> execute [CLR 1] [4,5,6]
-- 0
-- *Main> execute [JEQ 0 0 2, CLR 1, INC 1] [0,1,2]
-- 1

execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)

-- PROBLEM 8. YOUR CODE HERE
-- ---------------------------
-- start a program at instruction n instead of 0.  In other
-- words, change Jump instructions from (J x y t) to (J x y (t+n))
-- and leave all other instructions unchanged.

-- The transpose function takes an integer and a program and
-- transposes all instructions by that integer, by mapping the
-- transposeIns function across each instruction.
-- *Main> transpose 2 [INC 3, CLR 2, JEQ 0 1 0]
-- [INC {box = 3},CLR {box = 2},JEQ {box1 = 0, box2 = 1, target = 2}]
-- *Main> transpose 1 [JEQ 0 1 2, JEQ 3 4 5]
-- [JEQ {box1 = 0, box2 = 1, target = 3},JEQ {box1 = 3, box2 = 4, target = 6}]

transpose :: Int -> Program -> Program
transpose n p = map (\ins -> transposeIns n ins) p

-- The transposeIns function takes an integer and an instruction
-- and transposes that instruction by that integer n; that is, if
-- it is a JEQ instruction we increase the target by n and all
-- other instructions are left unchanged.
-- *Main> transposeIns 2 (JEQ 0 1 0)
-- JEQ {box1 = 0, box2 = 1, target = 2}
-- *Main> transposeIns 3 (CLR 1)
-- CLR {box = 1}

transposeIns :: Int -> Instruction -> Instruction
transposeIns n (JEQ x y t) = (JEQ x y (t+n))
transposeIns _ ins = ins

-- PROBLEM 9. YOUR CODE HERE
-- ---------------------------
-- join two programs together, so as to run one
-- after the other

-- The (*->*) function takes in two programs and joins them together,
-- by transposing the second program so it starts after the first and
-- then concatenating the two programs.
-- *Main> [INC 1] *->* [CLR 2]
-- [INC {box = 1},CLR {box = 2}]
-- *Main> [INC 1, JEQ 0 1 2] *->* [JEQ 3 4 5]
-- [INC {box = 1},JEQ {box1 = 0, box2 = 1, target = 2},JEQ {box1 = 3, box2 = 4, target = 7}]

(*->*) :: Program -> Program -> Program
p1 *->* p2 = p1 ++ transpose (length p1) p2


-- PROBLEM 10. YOUR CODE HERE
-- ---------------------------
-- program to compute B1 = B1 + B2

-- The adder program adds the contents of box 1 and 2 and places the
-- result in box 1, which is simply a more specific case of adderX.
-- *Main> execute adder [1,2,3]
-- 3
-- *Main> execute adder []
-- 0

adder :: Program
adder = adderX 2

-- The more general adderX program adds the contents of box 1 and x
-- and places the contents in box 1, by incrementing box 0 and box 1
-- until box 0 equals box x and then terminating. Unfortunately, I
-- couldn't implement adderX 1 without affecting the contents of
-- box 2.
-- *Main> execute (adderX 3) [1,2,3,4]
-- 4
-- *Main> execute (adderX 1) [3]
-- 6
-- *Main> execute (adderX 4) []
-- 0

adderX :: Int -> Program
adderX 1 = [INC 0, JEQ 0 1 3, JEQ 0 0 0, INC 0, INC 1, INC 1, JEQ 0 1 8, JEQ 0 0 3]
adderX x = [JEQ 0 x 4, INC 0, INC 1, JEQ 0 0 0]


-- PROBLEM 11. YOUR CODE HERE
-- ---------------------------
-- create a program to copy the contents of box m to box n (leave box m unchanged)

-- The copyBox function takes two integers and returns a program that
-- copies the first box to the second box, by first clearing the second
-- box and then incrementing it until it equals the first.
-- *Main> execute (copyBox 2 1) [1,2,3]
-- 2
-- *Main> execute (copyBox 1 1) [1]
-- 1
-- *Main> execute (copyBox 3 1) [1]
-- 0

copyBox :: Int -> Int -> Program
copyBox m n
  | n == m = []
  | otherwise = [CLR n, JEQ n m 4, INC n, JEQ 0 0 1]


-- PROBLEM 12. YOUR CODE HERE

-- ---------------------------
-- program to compute B1 = Bx + By

-- The addXY function takes two integers and returns a program which adds
-- box x to box y, by first copying box x to box 1 and then using adderX
-- to add box y to box 1.
-- *Main> execute (addXY 2 3) [1,2,3]
-- 5
-- *Main> execute (addXY 2 3) [1,2]
-- 2
-- *Main> execute (addXY 1 1) [2]
-- 4

addXY :: Int -> Int -> Program
addXY x y = (copyBox x 1) *->* (adderX y)


-- END OF TEMPLATE FILE