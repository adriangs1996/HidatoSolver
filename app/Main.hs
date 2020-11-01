module Main where

import           Lib
import           Hidato
import           Data.Time
import           HidatoBoard
import           System.Console.GetOpt
import           System.IO
import           Data.List
import           System.Exit
import           System.Environment
import Control.Concurrent

data Flag =
    Help
    | Generator Int
    | GenerateAndSolve
    | GenerateTestFile String
    | SolveFromFile String
    deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options =
    [ Option [] ["help"] (NoArg Help) "Print this help message"
    , Option ['g']
             ["generate"]
             (ReqArg (\x -> Generator (read x)) "COUNT")
             "Generate COUNT boards and shows them"
    , Option
        ['s']
        ["genandsolve"]
        (NoArg GenerateAndSolve)
        "Generate 1 board, prints it, solves it and print the solution"
    , Option ['t']
             ["gentestfile"]
             (ReqArg GenerateTestFile "FILE")
             "Generate 50 boards and save them in FILE"
    , Option ['f']
             ["solvefromfile"]
             (ReqArg SolveFromFile "FILE")
             "Read boards in file and solve them"
    ]

parse :: [String] -> IO [Flag]
parse argv = case getOpt Permute options argv of
    (args, _, []) -> do
        if Help `elem` args
            then do
                hPutStrLn stderr (usageInfo header options)
                exitWith ExitSuccess
            else return (nub args)

    (_, _, err) -> do
        hPutStrLn stderr (concat err ++ usageInfo header options)
        exitWith $ ExitFailure 1
    where header = "Usage: SudokuHidato [options]"


printM :: [IO ()] -> IO ()
printM [x     ] = x
printM (x : xs) = do
    x >> putStr "\n" >> printM xs
printM [] = putStrLn "Empty"

generateXBoard :: (Num a, Enum a) => a -> [IO BoardProblem]
generateXBoard n = map (\_ -> gen) [1 .. n]
  where
    gen = do
        time <- getCurrentTime
        let timeInt = floor $ utctDayTime time
        threadDelay 1000000
        return $ makeBoard $ generateBoard timeInt

generateXBoardString :: (Num a, Enum a) => a -> [IO [String]]
generateXBoardString n = map (\_ -> gen) [1 .. n]
  where
    gen = do
        time <- getCurrentTime
        let timeInt = floor $ utctDayTime time
        threadDelay 1000000
        return $ generateBoard timeInt


main :: IO ()
main = do
    args <- getArgs >>= parse
    manage args
  where
    manage [Generator x] =
        printM
            $ map
                  (\b -> do
                      y <- b
                      printCellMap $ cells y
                  )
            $ generateXBoard x
    manage [GenerateAndSolve] = do
        time <- getCurrentTime
        let timeInt = floor $ utctDayTime time
        let board = makeBoard $ generateBoard timeInt
        printCellMap $ cells board
        printCellMap . head $ bruteForceHidato board

    manage [GenerateTestFile file] = do
        handler <- openFile file WriteMode
        let boards = generateXBoardString 10
        mapM_
            (\b -> do
                brd <- b
                hPutStrLn handler $ show brd
            )
            boards
        hClose handler

    manage [SolveFromFile file] = do
        content <- readFile file
        mapM_ (printCellMap . head . bruteForceHidato . makeBoard . read)
            $ lines content

    manage [] = putStrLn "Done"

    manage (x:xs) = do
        manage [x]
        manage xs