{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}


module Lib where

import qualified Data.IntMap                   as I
import           Data.IntMap                    ( Key
                                                , IntMap
                                                )
import           Data.List                      ( foldl'
                                                , sort
                                                )
import           Data.Time.Clock                ( )
import           System.Random

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
tupIns :: Key -> Key -> Int -> IntMap (IntMap Int) -> IntMap (IntMap Int)
tupIns x y v m = I.insert x (I.insert y v (I.findWithDefault I.empty x m)) m

-- Devuelve el valor en la posicion (x,y) si existe,
-- de lo contrario Nothing
tupLookup :: Key -> Key -> IntMap (IntMap Int) -> Maybe Int
tupLookup x y m = I.lookup x m >>= I.lookup y

-- Parsear un string y convertirlo en la estructura
-- que representa el tablero
makeBoard :: [String] -> BoardProblem
makeBoard =
    (\x -> x { givens = dropWhile (<= 1) $ sort $ givens x })
        . foldl' f (Board I.empty 0 (0, 0) [])
        . concatMap (zip [0 ..])
        . zipWith (\y w -> map (y, ) $ words w) [0 ..]
  where
    -- Modificar el tablero en dependencia del valor v y las coordenadas (x, y)

    f bd (x, (y, v)) = if v == "."
        then bd
                  -- Insertar el valor v en dict[x][y]

        else Board (tupIns x y (read v) (cells bd))

                  -- Actualizar el maximo valor del tablero si es necesario

                   (if read v > endVal bd then read v else endVal bd)

                   -- Si encontramos el 1, guardar su posicion

                   (if v == "1" then (x, y) else onePos bd)

                   -- Agregar un valor prefijado

                   (read v : givens bd)

-- Representar un tablero de una manera agradable en una consola


printCellMap :: IntMap (IntMap Int) -> IO ()
printCellMap cellmap = putStrLn $ concat strings
  where
    maxPos = xyBy I.findMax maximum
    minPos = xyBy I.findMin minimum
    xyBy :: (forall a . IntMap a -> (Int, a)) -> ([Int] -> Int) -> (Int, Int)
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

cellMapToStr :: IntMap (IntMap Int) -> String
cellMapToStr cellmap = concat strings
  where
    maxPos = xyBy I.findMax maximum
    minPos = xyBy I.findMin minimum
    xyBy :: (forall a . IntMap a -> (Int, a)) -> ([Int] -> Int) -> (Int, Int)
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

isSolved :: IntMap (IntMap Int) -> (Int, Int) -> Int -> Bool
-- Comprobar si un tablero esta resuelto

isSolved pmap (x, y) maxVal = noEmpty && connected (x, y) 1
  where
    -- Comprobar que no haya 0 en el tablero

    noEmpty = not . elem 0 . concat . map I.elems $ I.elems pmap
    -- Comprobar que el tablero sea un camino Hamiltoniano

    connected (x1, y1) curVal
        | curVal == maxVal = True
        | otherwise = case pos of
            []              -> False
            [((x2, y2), _)] -> connected (x2, y2) (curVal + 1)
            _               -> False
      where
        around =
            [ (x1 - 1, y1 - 1)
            , (x1    , y1 - 1)
            , (x1 + 1, y1 - 1)
            , (x1 - 1, y1)
            , (x1 + 1, y1)
            , (x1 - 1, y1 + 1)
            , (x1    , y1 + 1)
            , (x1 + 1, y1 + 1)
            ]
        pos = filter ((== Just (curVal + 1)) . snd)
            $ map (\(i, j) -> ((i, j), tupLookup i j pmap)) around

generateNxMBoard :: Int -> Int -> [String]
generateNxMBoard n m = take n $ repeat $ concat $ take m $ repeat "0 "

-- >>> generateNxMBoard 5 3
-- ["0 0 0 ","0 0 0 ","0 0 0 ","0 0 0 ","0 0 0 "]

genRandTuple :: Int -> (Int, Int)
genRandTuple seed = (x, y)
  where
    gen = mkStdGen seed
    x   = abs $ head $ randoms gen :: Int
    y   = abs $ last $ take 2 $ randoms gen :: Int

-- >>> genRandTuple 1000
-- (1611434616111168504,961026615473336794)

