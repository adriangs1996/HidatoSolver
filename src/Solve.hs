module Solve
    ( makeHidatoBoard
    , boardTemplates
    )
where
import           Board                          ( Coordinates
                                                , Board(squares)
                                                , boardToStrings
                                                , setFree
                                                , initValue
                                                , getNewCoord
                                                , checkValidNumberCoord
                                                , getValidInitPos
                                                , getRandomMinMove
                                                )
import           PathTree                       ( PathTree
                                                    ( Empty
                                                    , Ptree
                                                    , pos
                                                    , value
                                                    )
                                                , emptyTree
                                                , isEmpty
                                                , isEmptyTree
                                                , treeToPosList
                                                , getNotEmptyChild
                                                , checkValidTree
                                                )
import           BoardGen                       ( createNewBoardFromConfig )
import           Utils                          ( Direction
                                                    ( NorthEast
                                                    , North
                                                    , South
                                                    , West
                                                    , East
                                                    , SouthWest
                                                    , NorthWest
                                                    , SouthEast
                                                    )
                                                , terOp
                                                , genRandInt
                                                , getBoundedRandInt
                                                , getRandomTuples
                                                , getListMiddle
                                                , popAtIndex
                                                )
import           Hidato                         ( bruteForceHidato )
import           System.Random                  ( )
import           Lib                            ( makeBoard )

-- #Region Boards config presets and board templates
boardTemplates :: [Board]
boardTemplates = [b1, b2, b3, b4]

board1 :: (Int, Int, [(Int, Int)])
board1 =
    ( 10
    , 10
    , [ (0, 0)
      , (0, 1)
      , (0, 2)
      , (0, 3)
      , (0, 6)
      , (0, 7)
      , (0, 8)
      , (0, 9)
      , (9, 0)
      , (9, 1)
      , (9, 2)
      , (9, 3)
      , (9, 6)
      , (9, 7)
      , (9, 8)
      , (9, 9)
      , (1, 0)
      , (1, 1)
      , (1, 2)
      , (1, 7)
      , (1, 8)
      , (1, 9)
      , (8, 0)
      , (8, 1)
      , (8, 2)
      , (8, 7)
      , (8, 8)
      , (8, 9)
      , (2, 0)
      , (2, 1)
      , (2, 8)
      , (2, 9)
      , (7, 0)
      , (7, 1)
      , (7, 8)
      , (7, 9)
      , (3, 0)
      , (3, 4)
      , (3, 5)
      , (3, 9)
      , (6, 0)
      , (6, 4)
      , (6, 5)
      , (6, 9)
      , (4, 3)
      , (4, 4)
      , (4, 5)
      , (4, 6)
      , (5, 3)
      , (5, 4)
      , (5, 5)
      , (5, 6)
      ]
    )

board2 :: (Int, Int, [(Int, Int)])
board2 =
    ( 9
    , 9
    , [ (0, 0)
      , (0, 1)
      , (0, 7)
      , (0, 8)
      , (1, 0)
      , (1, 1)
      , (1, 7)
      , (1, 8)
      , (2, 0)
      , (2, 1)
      , (2, 5)
      , (2, 6)
      , (2, 7)
      , (2, 8)
      , (3, 0)
      , (3, 1)
      , (3, 5)
      , (3, 6)
      , (7, 2)
      , (7, 3)
      , (7, 4)
      , (7, 5)
      , (7, 6)
      , (8, 2)
      , (8, 3)
      , (8, 4)
      , (8, 5)
      , (8, 6)
      ]
    )

board3 :: (Int, Int, [(Int, Int)])
board3 =
    ( 5
    , 7
    , [ (0, 0)
      , (0, 1)
      , (0, 3)
      , (0, 4)
      , (0, 5)
      , (0, 6)
      , (1, 0)
      , (1, 4)
      , (1, 5)
      , (1, 6)
      , (2, 5)
      , (2, 6)
      , (3, 0)
      , (4, 0)
      , (4, 1)
      , (4, 2)
      , (4, 6)
      ]
    )

board4 :: (Int, Int, [(Int, Int)])
board4 = (3, 3, [(0, 0), (0, 1), (2, 2)])

b1 :: Board
b1 = createNewBoardFromConfig board1

b2 :: Board
b2 = createNewBoardFromConfig board2

b3 :: Board
b3 = createNewBoardFromConfig board3

b4 :: Board
b4 = createNewBoardFromConfig board4



-- #Region The pollo del arroz con pollo
makeHidatoBoard :: Board -> Int -> Board
makeHidatoBoard brd seed = getUniqueBoard filledBrd tree seed1
  where
    tree      = getValidTree brd (genRandInt seed)
    seed1     = genRandInt seed
    filledBrd = fillBoardWithPath tree brd


-- private functions
getUniqueBoard :: (Eq a) => Board -> PathTree a -> Int -> Board
getUniqueBoard brd tree seed = getUniqueBoard' brd seed posList
    where posList = getListMiddle (treeToPosList tree)

getUniqueBoard' :: Board -> Int -> [Coordinates] -> Board
getUniqueBoard' brd _    []   = brd
getUniqueBoard' brd seed list = getUniqueBoard' newBrd newSeed newList
  where
    elemInd            = getBoundedRandInt seed (length list)
    (pos2Del, newList) = popAtIndex elemInd list
    tempBrd            = setFree pos2Del brd
    strBrd             = boardToStrings tempBrd
    solCount           = length (bruteForceHidato (makeBoard strBrd))
    newBrd             = terOp (solCount == 1) tempBrd brd
    newSeed            = genRandInt seed

buildNewTreeWarnsdorff :: Board -> Coordinates -> Int -> PathTree Int
buildNewTreeWarnsdorff brd coord seed =
    snd (buildTreeWarnsdorff Empty brd coord 1 (squares brd) seed)

buildTreeWarnsdorff
    :: PathTree Int
    -> Board
    -> Coordinates
    -> Int
    -> Int
    -> Int
    -> (Int, PathTree Int)
buildTreeWarnsdorff tree brd coord val max seed = if canInsert tree brd coord
    then if val == max
        then (1, emptyTree val 1 coord)
        else
            ( x
            , Ptree val
                    x
                    coord
                    treeNorth
                    treeSouth
                    treeWest
                    treeEast
                    treeSouthWest
                    treeNorthWest
                    treeSouthEast
                    treeNorthEast
            )
    else (0, Empty)
  where
    newValue            = val + 1
    minDir              = getRandomMinMove brd coord seed
    (x1, treeNorth    ) = terOp (minDir == North) newTree (0, Empty)
    (x2, treeSouth    ) = terOp (minDir == South) newTree (0, Empty)
    (x3, treeWest     ) = terOp (minDir == West) newTree (0, Empty)
    (x4, treeEast     ) = terOp (minDir == East) newTree (0, Empty)
    (x5, treeSouthWest) = terOp (minDir == SouthWest) newTree (0, Empty)
    (x6, treeNorthWest) = terOp (minDir == NorthWest) newTree (0, Empty)
    (x7, treeSouthEast) = terOp (minDir == SouthEast) newTree (0, Empty)
    (x8, treeNorthEast) = terOp (minDir == NorthEast) newTree (0, Empty)
    x                   = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
    brd'                = initValue coord brd val
    seed'               = genRandInt seed
    newTree             = buildTreeWarnsdorff Empty
                                              brd'
                                              (getNewCoord coord minDir)
                                              newValue
                                              max
                                              seed'

fillBoardWithPath :: PathTree Int -> Board -> Board
fillBoardWithPath Empty brd = brd
fillBoardWithPath tree  brd = if (isEmptyTree tree)
    then brd'
    else fillBoardWithPath tree' brd'
  where
    brd'  = initValue (pos tree) brd (value tree)
    tree' = getNotEmptyChild tree

getValidTree :: Board -> Int -> PathTree Int
getValidTree brd seed = if (checkValidTree tree) then tree else newTree
  where
    (_ : ((t1, t2) : _)) = getRandomTuples 2 seed
    seed1                = genRandInt (t1)
    seed2                = genRandInt (t2)
    initPos              = getValidInitPos brd seed2
    tree                 = buildNewTreeWarnsdorff brd initPos seed1
    newTree              = getValidTree brd seed2

canInsert :: PathTree Int -> Board -> Coordinates -> Bool
canInsert tree b coord = (checkValidNumberCoord coord b) && (isEmpty tree)
