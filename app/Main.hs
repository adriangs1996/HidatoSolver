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
    let sols = map printCellMap $ bruteForceHidato sampleBoard
    -- Esto imprime todas las soluciones a medida que las encuentra
    printM sols
    

sample :: [[Char]]
sample =
    [ "0 . . . . . . . ."
    , "0 0 . . . . . . ."
    , "0 0 0 . . . . . ."
    , "0 0 0 0 . . . . ."
    , "0 0 0 0 0 . . . ."
    , "0 0 1 0 0 0 . . ."
    , "0 0 0 0 0 0 0 . ."
    , "0 0 0 0 0 0 0 0 ."
    , "0 0 0 0 0 0 0 0 45"
    ]


