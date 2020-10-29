module Main where

import           Lib
import           Hidato
import Data.Time

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
    -- Obtener el tiempo actual
    time <- getCurrentTime
    -- convertir el tiempo actual a int
    let timeInt = floor $ utctDayTime time
    -- generar una tupla aleatoria utilizando el tiempo
    -- actual como semilla
    putStrLn (show $ genRandTuple timeInt)
    -- printM sols
    

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


