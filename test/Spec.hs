import           Hidato
import           Lib
import           Data.Time

board1 =
    [ " 0 33 35  0  0"
    , " 0 34 24 22  0"
    , " 0  0  0 21  0  0"
    , " 0 26  0 13 40 11"
    , "27  0  0  0  9 10  1"
    , ".  .   0  0 18  0  0"
    , ".  .  .  .   0  7  6  0"
    , ".  .  .  .  .  .   5  0"
    ]

board2 =
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

sol1 =
    "32 33 35 36 37         \n31 34 24 22 38         \n30 25 23 21 12 39      \n29 26 20 13 40 11      \n27 28 14 19  9 10  1   \n      15 16 18  8  2   \n            17  7  6  3\n                   5  4\n"


sol2 =
    " 7                        \n 6  8                     \n 5  9 11                  \n29  4 10 12               \n30 28  3  2 13            \n27 31  1 38 40 14         \n26 32 37 21 39 41 15      \n33 25 36 22 20 16 42 43   \n34 35 24 23 19 18 17 44 45\n"


test_case1 :: IO ()
test_case1 = do
    start <- getCurrentTime
    let brd      = makeBoard board1
    let solution = cellMapToStr $ head $ bruteForceHidato brd
    end <- getCurrentTime
    let elapsedTime = diffUTCTime end start
    if solution == sol1
        then putStrLn $ "Correct\nSolution computed in " ++ show elapsedTime
        else putStrLn
            (  "Wrong answer\n"
            ++ "Expected:\n"
            ++ show sol1
            ++ "\nGot:\n"
            ++ show solution
            )


test_case2 :: IO ()
test_case2 = do
    start <- getCurrentTime
    let brd      = makeBoard board2
    let solution = cellMapToStr $ head $ bruteForceHidato brd
    end <- getCurrentTime
    let elapsedTime = diffUTCTime end start
    if solution == sol2
        then putStrLn $ "Correct\nSolution computed in " ++ show elapsedTime
        else putStrLn
            (  "Wrong answer\n"
            ++ "Expected:\n"
            ++ show sol1
            ++ "\nGot:\n"
            ++ show solution
            )


main :: IO ()
main = do
    test_case1
    test_case2

