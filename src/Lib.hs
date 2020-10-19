{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}


module Lib where

-- import           Data.Char                      ( digitToInt
--                                                 , isDigit
--                                                 )
-- import           Data.List                      ( (\\)
--                                                 , foldl'
--                                                 )

-- -- Una celda o contiene un valor o contiene una lista
-- -- de posibles valores o esta simplemente vacia
-- data Cell = Fixed Int | Possibles [Int] | Empty  deriving (Show, Eq)

-- type Row = [Cell]
-- type Grid = [Row]

-- -- Funcion para convertir un str en un int
-- strToInt :: [Char] -> Int
-- strToInt ('-' : s) = negate $ foldl' step 0 s
--     where step acc c = acc * 10 + digitToInt c
-- strToInt s = foldl' step 0 s where step acc c = acc * 10 + digitToInt c

-- strIsNumber :: Foldable t => t Char -> Bool
-- strIsNumber = all isDigit

-- -- >>> strToInt "-10598"
-- -- -10598

-- -- Funcion para dividir una lista en sublistas de dimension
-- -- fija
-- chunkOf :: Int -> [a] -> [[a]]
-- chunkOf n xs = loop [] xs
--   where
--     loop acc [] = acc
--     loop acc ys = let acc' = acc ++ [(take n ys)] in loop acc' (drop n ys)

-- -- Funcion para extraer todos las sublistas de dimension n de una lista
-- allChunksOf :: Int -> [a] -> [[a]]
-- allChunksOf n xs = loop [] xs
--   where
--     loop acc [] = acc
--     loop acc ys
--         | length ys >= n
--         = let acc' = acc ++ [(take n ys)] in loop acc' (drop 1 ys)
--         | otherwise
--         = acc

-- -- >>> chunkOf 3 [1, 2, 3, 1, 2, 3, 1, 2 ,3]
-- -- >>> allChunksOf [1,2,3,4,5,6,7,8] 3
-- -- [[1,2,3],[1,2,3],[1,2,3]]

-- -- [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8]]


-- -- Parsear un tablero a partir de un String y convertirlo
-- -- en un Grid
-- readGrid :: String -> Int -> Int -> Int -> Maybe Grid
-- readGrid s n minVal maxVal = traverse (traverse readCell) . chunkOf n $ words s
--   where
--     readCell "." = Just $ Possibles [minVal .. maxVal]
--     readCell "x" = Just Empty
--     readCell c | strIsNumber c = Just . Fixed . strToInt $ c
--                | otherwise     = Nothing

-- -- Mostrar un tablero de manera humanamente legible
-- -- en consola
-- showGrid :: Grid -> String
-- showGrid = unlines . map (unwords . map showCell)
--   where
--     showCell (Fixed x) | x < 10    = show x ++ " "
--                        | otherwise = show x
--     showCell (Possibles _) = "--"
--     showCell _             = "  "

-- -- Mostrar las distintas posibilidades en cada posicion
-- -- del tablero.
-- showPossibilities :: Int -> Grid -> String
-- showPossibilities maxVal = unlines . map (unwords . map showCell)
--   where
--     showCell (Fixed x) =
--         show x ++ "                                               "
--     showCell (Possibles xs) =
--         (++ "]")
--             . foldl'
--                   (\acc x -> acc ++ if x `elem` xs then show x ++ " " else " ")
--                   "["
--             $ [1 .. maxVal]
--     showCell _ = "[]"

-- -- >>> Just g = readGrid ". 33 35 . . x x x . . 24 22 . x x x . . . 21 . . x x . 26 . 13 40 11 x x 27 . . . 9 . 1 x x x . . 18 . . x x x x x . 7 . . x x x x x x 5 ." 8 1 40
-- -- >>> showGrid g
-- -- "-- 33 35 -- --         \n-- -- 24 22 --         \n-- -- -- 21 -- --      \n-- 26 -- 13 40 11      \n27 -- -- -- 9  -- 1    \n      -- -- 18 -- --   \n            -- 7  -- --\n                  5  --\n"

-- -- Funcion para eliminar posibles valores de una
-- -- fila. Esto tiene sentido pues cada valor es
-- -- unico en el tablero
-- pruneCells :: Row -> Maybe Row
-- pruneCells cells = traverse pruneCell cells
--   where
--     fixeds = [ x | Fixed x <- cells ]
--     pruneCell (Possibles xs) = case xs \\ fixeds of
--         []  -> Nothing
--         [y] -> Just $ Fixed y
--         ys  -> Just $ Possibles ys
--     pruneCell x = Just x

-- -- Eliminar un valor de todos las posibilidades en el tablero
-- pruneValueFromGrid :: Int -> Grid -> Maybe Grid
-- pruneValueFromGrid val grid = traverse (traverse pruneCell) grid
--   where
--     pruneCell (Possibles xs) = case xs \\ [val] of
--         []  -> Nothing
--         [y] -> Just $ Fixed y
--         ys  -> Just $ Possibles ys
--     pruneCell x = Just x

-- -- Generar todos los posibles subgrids de dimension 9 en
-- -- el tablero
-- subGridsToRow :: Grid -> Grid
-- subGridsToRow =
--     concatMap
--             (\rows ->
--                 let [r1, r2, r3] = map (allChunksOf 3) rows
--                 in  zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3
--             )
--         . allChunksOf 3

-- predecessor :: Eq a => a -> [a] -> a
-- predecessor x xs = last (takeWhile (/= x) xs)

-- successor :: Eq a => a -> [a] -> a
-- successor x xs = last $ take (length (takeWhile (/= x) xs) + 2) xs

-- indexOf :: Eq a => a -> [a] -> Int
-- indexOf x xs = index
--   where
--     l = length (takeWhile (/= x) xs)
--     index | l == length xs = -1
--           | otherwise      = l

-- -- >>> predecessor 0 [1, 2, 3, 4, 5 ,6]
-- -- 6

-- -- >>> successor 4 [1, 2 ,3 ,4 ,5 ,6]
-- -- 5

-- -- >>> indexOf 0 [1, 2, 3, 4, 5, 6]
-- -- -1

-- -- Saber las posibles celdas que conectan dos numeros intercalados
-- -- (7 y 9 son intercalados)
-- connectingCells :: Row -> Row -> Row -> [Cell]
-- connectingCells r1 [] [] = concatMap cells fixeds
--   where
--     fixeds =
--         [ [Fixed x, Fixed y]
--         | Fixed x <- r1
--         , Fixed y <- r1
--         , abs (x - y) == 2
--         , x /= y
--         ]
--     cells [x, y] = if indexOf x r1 < indexOf y r1
--         then (tail . takeWhile (/= y) . takeWhile (/= x)) r1
--         else (tail . takeWhile (/= x) . takeWhile (/= y)) r1

-- connectingCells _ _ _ = undefined


import qualified Data.IntMap                   as I
import           Data.IntMap                    ( Key
                                                , IntMap
                                                )
import           Data.List                      ( foldl'
                                                , sort
                                                )
import           Data.Time.Clock                ( )



data BoardProblem = Board
  {
    -- El tablero es una matriz de enteros que se puede representar
    -- como un diccionario de enteros en diccionario de enteros
    -- cells[x][y] = Int
    cells :: IntMap (IntMap Int)
    -- Mayor valor del tablero
  , endVal :: Int
    -- Posicion del 1
  , onePos :: (Int, Int)
    -- Valores que hemos computado hasta el momento
  , givens :: [Int]
  } deriving (Show, Eq)

-- Insertar una nueva tupla en el diccionario.
-- Esta funcion no es monadica, por lo que devuelve un
-- nuevo diccionario tal que m[x][y] = v
tupIns :: Key -> Key -> a -> IntMap (IntMap a) -> IntMap (IntMap a)
tupIns x y v m = I.insert x (I.insert y v (I.findWithDefault I.empty x m)) m

-- Devuelve el valor en la posicion (x,y) si existe,
-- de lo contrario Nothing
tupLookup :: Key -> Key -> IntMap (IntMap b) -> Maybe b
tupLookup x y m = I.lookup x m >>= I.lookup y

-- Parsear un string y convertirlo en la estructura
-- que representa el tablero
makeBoard :: [String] -> BoardProblem
makeBoard =
    (\x -> x { givens = dropWhile (<= 1) $ sort $ givens x })
        . foldl' --'
                 f (Board I.empty 0 (0, 0) [])
        . concatMap (zip [0 ..])
        . zipWith (\y w -> map (y, ) $ words w) [0 ..]
  where
    f bd (x, (y, v)) = if v == "."
        then bd
        else Board (tupIns x y (read v) (cells bd))
                   (if read v > endVal bd then read v else endVal bd)
                   (if v == "1" then (x, y) else onePos bd)
                   (read v : givens bd)


printCellMap :: (Ord b, Num b, Show b) => IntMap (IntMap b) -> IO ()
printCellMap cellmap = putStrLn $ concat strings
  where
    maxPos = xyBy I.findMax maximum
    minPos = xyBy I.findMin minimum
    xyBy :: (forall a. IntMap a -> (Int, a)) -> ([Int] -> Int) -> (Int, Int)
    xyBy a b = (fst (a cellmap), b $ map (fst . a . snd) $ I.toList cellmap)
    strings = map
        f
        [ (x, y)
        | y <- [snd minPos .. snd maxPos]
        , x <- [fst minPos .. fst maxPos]
        ]
    f (x, y) =
        let z = if x == fst maxPos then "\n" else " "
        in  case tupLookup x y cellmap of
                Nothing -> "  " ++ z
                Just n  -> (if n < 10 then ' ' : show n else show n) ++ z
