{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Hidato where

import           Lib
import           Data.IntMap                    ( IntMap )

bruteForceHidato :: BoardProblem -> [IntMap (IntMap Int)]
bruteForceHidato brd = h 2 (cells brd) (onePos brd) (givens brd)
  where
    h nval pmap (x, y) gs
        | isSolved pmap (onePos brd) (endVal brd) = [pmap]
        | nval == head gs = if null nvalAdj
            then []
            else h (nval + 1) pmap (fst $ head nvalAdj) (tail gs)
        | not $ null nvalAdj = h (nval + 1) pmap (fst $ head nvalAdj) gs
        | otherwise = hEmptyAdj
      where
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
        lkdUp   = map (\(x, y) -> ((x, y), tupLookup x y pmap)) around
        nvalAdj = filter ((== Just nval) . snd) lkdUp
        hEmptyAdj =
            concatMap
                    (\((nx, ny), _) ->
                        h (nval + 1) (tupIns nx ny nval pmap) (nx, ny) gs
                    )
                $ filter ((== Just 0) . snd) lkdUp