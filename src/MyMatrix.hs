module MyMatrix (
    MyMatrix,
    newMatrixDefault,
    newMatrixFromList,
    setElem,
    getElem,
    rowsCount,
    colsCount,
    prettyMatrix,
    matrixToStrings
) where

import Data.List

--Data types

data MyMatrix a = Matrix {
                  rowsCount :: Int
                , colsCount :: Int
                , elements :: [[a]]
                } deriving(Eq)

instance (Show a) => Show (MyMatrix a) where
    show x = prettyMatrix x show


--Public functions

newMatrixFromList :: [[a]] -> MyMatrix a
newMatrixFromList x = Matrix (length x) (length (head x)) x

newMatrixDefault :: Int -> Int -> a -> MyMatrix a
newMatrixDefault r c x = Matrix r c (replicate r (replicate c x))

setElem :: MyMatrix a -> a -> Int -> Int -> MyMatrix a
setElem (Matrix _ _ e) a r c = newMatrixFromList (setElem2 e a r c)

getElem :: MyMatrix a -> Int -> Int -> a
getElem (Matrix _ _ e) i j  = e !! i !! j

prettyMatrix :: (Show a) => MyMatrix a -> (a -> String) -> String
prettyMatrix m f = prettyMatrix2 (elements m) f

matrixToStrings :: (Show a) => MyMatrix a -> (a -> String) -> [String]
matrixToStrings m f = matrixToStrings2 (elements m) f

--Private functions

setElem2 :: [[a]] -> a -> Int -> Int -> [[a]]
setElem2 [] _ _ _     = []
setElem2 (x:xs) a 0 c = (setElemList x a c):xs
setElem2 (x:xs) a r c = x:(setElem2 xs a r' c)
                              where r' = r - 1

insertElem :: [a] -> a -> Int -> [a]
insertElem [] x _            = [x]
insertElem xs elem 0         = elem:xs
insertElem (x:xs) elem index = x:(insertElem xs elem index')
                                where index' = index -1

setElemList :: [a] -> a -> Int -> [a]
setElemList [] _ _            = []
setElemList (_:xs) elem 0     = elem:xs
setElemList (x:xs) elem index = x:(setElemList xs elem index')
                            where index' = index - 1

prettyMatrix2 :: (Show a) => [[a]] -> (a -> String) -> String
prettyMatrix2 [] _     = ""
prettyMatrix2 (x:[]) f = prettyList x f
prettyMatrix2 (x:xs) f = (prettyList x f) ++ "\n" ++ prettyMatrix2 xs f

prettyList :: (Show a) => [a] -> (a -> String) -> String
prettyList [] _     = ""
prettyList (x:[]) f = f x
prettyList (x:xs) f = f x ++ " " ++ prettyList xs f

matrixToStrings2 :: (Show a) => [[a]] -> (a -> String) -> [String]
matrixToStrings2 [] _     = []
matrixToStrings2 (x:xs) f = ((prettyList x f):(matrixToStrings2 xs f))
