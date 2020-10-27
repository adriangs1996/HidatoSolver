module Main where

import           Lib
import           Hidato

printM :: [IO ()] -> IO ()
printM [x] = x
printM (x:xs) = do
    x >> putStr "\n" >> printM xs
printM [] = putStrLn "Empty"

main :: IO ()
main = do
    let sampleBoard = makeBoard sample
    printCellMap $ cells sampleBoard
    let solved = bruteForceHidato sampleBoard
    printCellMap $ head solved
    putStrLn (show (isSolved (head solved) (onePos sampleBoard) (endVal sampleBoard)))
    let sols = map printCellMap $ bruteForceHidato sampleBoard
    printM sols
    

sample :: [[Char]]
sample =
    [ "0 "
    , "0 8"
    , "0 0 11"
    , "29 0 10 0"
    , "30 0 0 0 0"
    , "0 31 1 38 0 0"
    , "0 32 0 0 39 41 0"
    , "0 0 0 22 0 0 42 0"
    , "0 0 0 0 0 0 0 44 45"
    ]


