{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Hidato where

import           Lib
import           Data.IntMap                    ( IntMap )

bruteForceHidato :: BoardProblem -> [IntMap (IntMap Int)]
bruteForceHidato brd =
    -- Resolver un tablero es intentar un recorrido en DFS
    -- partiendo desde la posicion del 1, intentando poner
    -- un 2. 
    solve 2 (cells brd) (onePos brd) (givens brd)
  where
    -- Resolver un tablero a partir de la posicion (x, y)
    -- donde tenemos que colocar como proximo valor nval
    -- e inicialmente tenemos los valores gs puestos en el tablero
    solve nval pmap (x, y) gs
        -- Si el tablero ya esta solucionado, devolvemos el tablero
        | isSolved pmap (onePos brd) (endVal brd) = [pmap]
        -- Si el valor que queremos colocar es un valor prefijado
        -- entonces vemos las posibles casillas a las que podemos movernos
        -- e intetamos poner el proximo valor. Si no hay casillas disponibles
        -- entonces no tenemos solucion.
        | nval == head gs = if null nvalAdj
            -- No hay solucion
            then []
            -- Resolver el mismo tablero, pero intentado poner un valor mas
            -- en alguna de las casillas adyacentes al valor prefijado
            else solve (nval + 1) pmap (fst $ head nvalAdj) (tail gs)
        -- Mismo caso, pero sin consumir valores prefijados
        | not $ null nvalAdj = solve (nval + 1) pmap (fst $ head nvalAdj) gs
        -- Explorar el resultado de poner nval en alguno de los vecinos disponibles
        -- de (x, y) y devolver la lista con los resultados. Esto garantiza que el algoritmo
        -- devuelva todas las soluciones de un tablero, y la evaluacion lazy de Haskell permite
        -- ir obteniendolas a demanda.
        | otherwise = hEmptyAdj
      where
        -- Definir las vecindades del par (x, y)
        around =
            [ (x - 1, y - 1)
            , (x    , y - 1)
            , (x + 1, y - 1)
            , (x - 1, y)
            , (x + 1, y)
            , (x - 1, y + 1)
            , (x    , y + 1)
            , (x + 1, y + 1)
            ]
        -- Lista de vecinos de (x, y)
        lkdUp   = map (\(x, y) -> ((x, y), tupLookup x y pmap)) around
        -- Lista con los vecinos que contienen el valor nval
        nvalAdj = filter ((== Just nval) . snd) lkdUp
        -- Lista de soluciones insertando el valor nval en cada una de las 
        -- vecindades disponibles del par (x, y)
        hEmptyAdj =
            concatMap
                    (\((nx, ny), _) ->
                        solve (nval + 1) (tupIns nx ny nval pmap) (nx, ny) gs
                    )
                $ filter ((== Just 0) . snd) lkdUp