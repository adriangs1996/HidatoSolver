module Main where

import           Lib
import           Hidato
import           Data.Maybe                     ( fromJust )

main :: IO ()
main = do
    let sampleBoard = makeBoard sample
    printCellMap $ cells sampleBoard
    printCellMap $ fromJust $ bruteForceHidato sampleBoard

sample :: [[Char]]
sample =
    [ " 0 33 35  0  0"
    , " 0  0 24 22  0"
    , " 0  0  0 21  0  0"
    , " 0 26  0 13 40 11"
    , "27  0  0  0  9  0  1"
    , ".  .   0  0 18  0  0"
    , ".  .  .  .   0  7  0  0"
    , ".  .  .  .  .  .   5  0"
    ]
