module Utils where

import Data.Char
import System.Random
import Data.Time

data Direction = North | South | West | East | NorthWest | NorthEast | SouthWest | SouthEast | None deriving(Eq, Show)

shortDirShow :: Direction -> String
shortDirShow North = " N"
shortDirShow South = " S"
shortDirShow West = " W"
shortDirShow East = " E"
shortDirShow NorthWest = "NW"
shortDirShow NorthEast = "NE"
shortDirShow SouthWest = "SW"
shortDirShow SouthEast = "SE"
shortDirShow None = "  "

type Coordinates = (Int,Int)

mapDirToTuple :: Direction -> (Int,Int)
mapDirToTuple x 
            |x == North = ((-1),0)
            |x == South = (1,0)
            |x == West = (0, (-1))
            |x == East = (0,1)
            |x == NorthEast = ((-1),1)
            |x == NorthWest = ((-1),(-1))
            |x == SouthEast = (1,1)
            |x == SouthWest = (1,(-1))
            |x == None = (0,0)

terOp :: Bool -> a -> a -> a
terOp True x _ = x
terOp False _ y = y

directions = [North, South, West, East, NorthWest, NorthEast, SouthWest, SouthEast, None]

genRandInt :: Int -> Int
genRandInt seed = fst $ random (mkStdGen seed)

genRandTuple :: Int -> (Int, Int)
genRandTuple seed = (x, y)
  where
        x = genRandInt seed
        y = genRandInt x

getBoundedRandInt :: Int -> Int -> Int
getBoundedRandInt seed bound = mod (genRandInt seed) bound

getRandomNumbers :: Int -> Int -> [Int]
getRandomNumbers max seed = getRandomNumbers' max seed 1 []

getRandomNumbers' :: Int -> Int -> Int -> [Int] -> [Int]
getRandomNumbers' max seed acc list = if acc >= max 
                                    then list ++ [max]
                                    else getRandomNumbers' max seed' acc' list'
                                        where
                                             acc' = acc + (getBoundedRandInt seed 4) + 1
                                             seed' = genRandInt seed
                                             list' = list ++ [acc]

getRandomElementFromList :: [a] -> Int -> a
getRandomElementFromList list seed = list !! ind
                              where
                                   ind = getBoundedRandInt seed (length list)

getRandomTuples :: Int -> Int -> [(Int,Int)]
getRandomTuples amnt seed = zip xs j
                         where 
                              (x:xs) = take (amnt + 1) (randoms (mkStdGen seed))
                              j = take amnt (randoms (mkStdGen x))

getListMiddle :: [a] -> [a]
getListMiddle x = drop 1 $ init x

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex _ [] = []
removeAtIndex x list = he ++ (ta)
                        where 
                            (he, (_:ta)) = splitAt x list

popAtIndex :: Int -> [a] -> (a, [a])
popAtIndex x list = (t, he ++ (ta))
                        where 
                            (he, (t:ta)) = splitAt x list


myHead :: [a] -> a
myHead [] = error "Head a lista vacia"
myHead (x:_) = x

--getRandSeed :: Int
--getRandSeed = seed
--                where
--                    seed = do
--                            time <- getCurrentTime
--                            intTime <- floor $ utctDayTime time
--                            return intTime