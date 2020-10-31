module Solve (
    makeHidatoBoard,
    boardTemplates
) where
import Board
import PathTree
import BoardGen
import Utils
import Hidato
import System.Random
import Lib (makeBoard)

-- #Region Boards config presets and board templates
boardTemplates = [b1,b2,b3,b4]

board1 = (10, 10, [(0,0),
                   (0,1),
                   (0,2),
                   (0,3),
                   (0,6),
                   (0,7),
                   (0,8),
                   (0,9),
                   (9,0),
                   (9,1),
                   (9,2),
                   (9,3),
                   (9,6),
                   (9,7),
                   (9,8),
                   (9,9),
                   (1,0),
                   (1,1),
                   (1,2),
                   (1,7),
                   (1,8),
                   (1,9),
                   (8,0),
                   (8,1),
                   (8,2),
                   (8,7),
                   (8,8),
                   (8,9),
                   (2,0),
                   (2,1),
                   (2,8),
                   (2,9),
                   (7,0),
                   (7,1),
                   (7,8),
                   (7,9),
                   (3,0),
                   (3,4),
                   (3,5),
                   (3,9),
                   (6,0),
                   (6,4),
                   (6,5),
                   (6,9),
                   (4,3),
                   (4,4),
                   (4,5),
                   (4,6),
                   (5,3),
                   (5,4),
                   (5,5),
                   (5,6)])

board2 = (9, 9, [(0,0),
                 (0,1),
                 (0,7),
                 (0,8),
                 (1,0),
                 (1,1),
                 (1,7),
                 (1,8),
                 (2,0),
                 (2,1),
                 (2,5),
                 (2,6),
                 (2,7),
                 (2,8),
                 (3,0),
                 (3,1),
                 (3,5),
                 (3,6),
                 (7,2),
                 (7,3),
                 (7,4),
                 (7,5),
                 (7,6),
                 (8,2),
                 (8,3),
                 (8,4),
                 (8,5),
                 (8,6)])

board3 = (5, 7, [(0,0),
                 (0,1),
                 (0,3),
                 (0,4),
                 (0,5),
                 (0,6),
                 (1,0),
                 (1,4),
                 (1,5),
                 (1,6),
                 (2,5),
                 (2,6),
                 (3,0),
                 (4,0),
                 (4,1),
                 (4,2),
                 (4,6)])

board4 = (3,3, [(0,0), (0,1), (2,2)])

b1 = createNewBoardFromConfig board1

b2 = createNewBoardFromConfig board2

b3 = createNewBoardFromConfig board3

b4 = createNewBoardFromConfig board4



-- #Region The pollo del arroz con pollo
makeHidatoBoard :: Board -> Int -> Board
makeHidatoBoard brd seed = getUniqueBoard filledBrd tree seed1
                         where
                              tree = getValidTree brd (genRandInt seed)
                              seed1 = genRandInt seed
                              filledBrd = fillBoardWithPath tree brd


-- private functions
getUniqueBoard :: (Eq a) => Board -> PathTree a -> Int -> Board
getUniqueBoard brd tree seed = getUniqueBoard' brd seed posList
                         where posList = getListMiddle (treeToPosList tree)

getUniqueBoard' :: Board -> Int -> [Coordinates] -> Board
getUniqueBoard' brd _ [] = brd
getUniqueBoard' brd seed list = getUniqueBoard' newBrd newSeed newList
                              where 
                                   elemInd = getBoundedRandInt seed (length list)
                                   (pos2Del, newList) = popAtIndex elemInd list
                                   tempBrd = setFree pos2Del brd
                                   strBrd = boardToStrings tempBrd
                                   solCount = length (bruteForceHidato (makeBoard strBrd))
                                   newBrd = terOp (solCount == 1) tempBrd brd
                                   newSeed = genRandInt seed

buildNewTreeWarnsdorff :: Board -> Coordinates -> Int -> PathTree Int
buildNewTreeWarnsdorff brd coord seed = snd (buildTreeWarnsdorff Empty brd coord 1 (squares brd) seed)

buildTreeWarnsdorff :: PathTree Int -> Board -> Coordinates -> Int -> Int -> Int -> (Int, PathTree Int)
buildTreeWarnsdorff tree brd coord val max seed = if canInsert tree brd coord
                                                  then if val == max
                                                       then (1, emptyTree val 1 coord)
                                                       else (x, Ptree val x coord treeNorth treeSouth treeWest treeEast treeSouthWest treeNorthWest treeSouthEast treeNorthEast)
                                                  else (0, Empty)
                                                       where
                                                            newValue = val + 1
                                                            minDir = getRandomMinMove brd coord seed
                                                            (x1, treeNorth) = terOp (minDir == North) newTree (0, Empty)
                                                            (x2, treeSouth) = terOp (minDir == South) newTree (0, Empty)
                                                            (x3, treeWest) = terOp (minDir == West) newTree (0, Empty)
                                                            (x4, treeEast) = terOp (minDir == East) newTree (0, Empty)
                                                            (x5, treeSouthWest) = terOp (minDir == SouthWest) newTree (0, Empty)
                                                            (x6, treeNorthWest) = terOp (minDir == NorthWest) newTree (0, Empty)
                                                            (x7, treeSouthEast) = terOp (minDir == SouthEast) newTree (0, Empty)
                                                            (x8, treeNorthEast) = terOp (minDir == NorthEast) newTree (0, Empty)
                                                            x = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
                                                            brd' = initValue coord brd val
                                                            seed' = genRandInt seed
                                                            newTree = buildTreeWarnsdorff Empty brd' (getNewCoord coord minDir) newValue max seed'
                                                       
fillBoardWithPath :: PathTree Int -> Board -> Board
fillBoardWithPath Empty brd = brd
fillBoardWithPath tree brd = if (isEmptyTree tree) then brd'
                             else fillBoardWithPath tree' brd'
                            where 
                                  brd' = initValue (pos tree) brd (value tree)
                                  tree' = getNotEmptyChild tree

getValidTree :: Board -> Int -> PathTree Int
getValidTree brd seed = if (checkValidTree tree) then tree else newTree
                              where
                                   ( _ : ( (t1,t2) : _ ) ) = getRandomTuples 2 seed
                                   seed1 = genRandInt (t1)
                                   seed2 = genRandInt (t2)
                                   initPos = getValidInitPos brd seed2
                                   tree =  buildNewTreeWarnsdorff brd initPos seed1
                                   newTree = getValidTree brd seed2

canInsert :: PathTree Int -> Board -> Coordinates -> Bool
canInsert tree b coord = (checkValidNumberCoord coord b) && (isEmpty tree)



-- #Region Brute Force Tree Generation
-- buildNewTree :: Board -> Coordinates -> PathTree Int
-- buildNewTree brd coord = snd (buildTree Empty brd coord 1 (squares brd))

-- buildTree :: PathTree Int -> Board -> Coordinates -> Int -> Int-> (Int, PathTree Int)
-- buildTree tree brd coord val max = if canInsert tree brd coord 
--                                    then if val == max
--                                         then (1, emptyTree val 1 coord)
--                                         else (x, Ptree val x coord treeNorth treeSouth treeWest treeEast treeSouthWest treeNorthWest treeSouthEast treeNorthEast)
--                                    else (0, Empty)
--                                                 where
--                                                     newValue = val + 1
--                                                     (x1, treeNorth) = buildTree Empty brd' (getNewCoord coord North) newValue max
--                                                     (x2, treeSouth) = buildTree Empty brd' (getNewCoord coord South) newValue max
--                                                     (x3, treeWest) = buildTree Empty brd' (getNewCoord coord West) newValue max
--                                                     (x4, treeEast) = buildTree Empty brd' (getNewCoord coord East) newValue max
--                                                     (x5, treeSouthWest) = buildTree Empty brd' (getNewCoord coord SouthWest) newValue max
--                                                     (x6, treeNorthWest) = buildTree Empty brd' (getNewCoord coord NorthWest) newValue max
--                                                     (x7, treeSouthEast) = buildTree Empty brd' (getNewCoord coord SouthEast) newValue max
--                                                     (x8, treeNorthEast) = buildTree Empty brd' (getNewCoord coord NorthEast) newValue max
--                                                     x = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8
--                                                     brd' = initValue coord brd val


-- #Region Old method for filling the board randomly from a solution tree
-- buildBoard' :: PathTree Int -> Int -> Board -> Board
-- buildBoard' tree seed brd = buildBoard'' tree brd (getRandomNumbers (squares brd) seed)

-- buildBoard'' :: PathTree Int -> Board -> [Int] -> Board
-- buildBoard'' _ brd [] = brd
-- buildBoard'' tree@(Ptree val _ pos _ _ _ _ _ _ _ _) brd list@(x:xs) = if val == x 
--                                                                       then buildBoard'' tree' brd' xs
--                                                                       else buildBoard'' tree' brd list
--                                                                       where
--                                                                            tree' = getNotEmptyChild tree
--                                                                            brd' = initValue pos brd val
           

-- #Region Not very efficient methods for generating a unique board !!do not use!! P.D: the second one is the slowest
-- setUniqueBoard :: Board -> Int -> Board
-- setUniqueBoard brd seed = setUniqueBoard' brd tuples
--                                         where
--                                              tuples = getRandomTuples (squares brd) seed

-- setUniqueBoard' :: Board -> [(Int, Int)] -> Board
-- setUniqueBoard' brd [] = brd
-- setUniqueBoard' brd (x:xs) = setUniqueBoard' brd' xs
--                                         where
--                                              pos2Del = extractPos brd x
--                                              tempBrd = if (checkValidDeletion pos2Del brd) then (setFree pos2Del brd) else brd
--                                              strBrd = boardToStrings tempBrd
--                                              solution = bruteForceHidato (makeBoard strBrd)
--                                              brd' = if (length solution == 1) then tempBrd else brd


-- setUniqueBoard2 :: Board  -> Board -> Int -> Board
-- setUniqueBoard2 brd filled seed = setUniqueBoard2' brd filled seed solCount
--                               where 
--                                    strBrd = boardToStrings brd
--                                    solCount = length (bruteForceHidato (makeBoard strBrd)) 

-- setUniqueBoard2' :: Board -> Board -> Int -> Int -> Board
-- setUniqueBoard2' brd _ _ 1 = brd
-- setUniqueBoard2' brd filledBrd seed solCount = setUniqueBoard2' brd' filledBrd newSeed solCount'
--                          where
--                               pos2Add = getValidInitPos brd seed
--                               --pos2Add = extractPos brd pos
--                               value = getCellValue pos2Add filledBrd
--                               tempBrd = if (checkValidNumberCoord pos2Add brd) then (initValue pos2Add brd value) else brd
--                               strBrd = boardToStrings tempBrd
--                               solCount' = length (bruteForceHidato (makeBoard strBrd))
--                               brd' = if (solCount' == 1) then tempBrd else brd
--                               newSeed = genRandInt seed



-- #Region Old method for generating hidato board and it's disastrous alternative
-- makeHidatoBoard :: Board -> Int -> Board
-- makeHidatoBoard brd seed = setUniqueBoard filledBrd newSeed2
--                               where 
--                                    tup = genRandTuple seed
--                                    newSeed1 = genRandInt (fst tup)
--                                    newSeed2 = genRandInt (snd tup)
--                                    tree = getValidTree brd newSeed1
--                                    filledBrd = fillBoardWithPath tree brd
                                   
-- makeHidatoBoard2 :: Board -> Int -> Board
-- makeHidatoBoard2 brd seed = setUniqueBoard2 brd' filledBrd newSeed2
--                               where 
--                                    tup = genRandTuple seed
--                                    newSeed1 = genRandInt (fst tup)
--                                    newSeed2 = genRandInt (snd tup)
--                                    tree = getValidTree brd newSeed1
--                                    filledBrd = fillBoardWithPath tree brd
--                                    brd' = buildBoard' tree newSeed2 brd
