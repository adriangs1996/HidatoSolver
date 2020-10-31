module Board (
    Board,
    board,
    squares,
    createEmptyBoard,
    setWall,
    setBusy,
    setFree,
    setValue,
    updateMoves,
    editMoves,
    decreaseMoves,
    setWalls,
    initValue,
    getCell,
    getCellState,
    getCellValue,
    getCellMoves,
    isCellFree,
    isCellBusy,
    isCellWall,
    Coordinates,
    getNewCoord,
    checkValidCoord,
    checkValidNumberCoord,
    prettyBoard,
    prettyBoard2,
    boardToStrings,
    getRowsCount,
    getColsCount,
    getValidInitPos,
    extractPos,
    checkValidDeletion,
    getMinMoveWarnsdorff,
    getRandomMinMove,
    getAllMinMoves
)
where
import MyMatrix
import Cell
import Utils

-- Types

data Board = Board {
             board :: MyMatrix BoardCell
            ,squares :: Int 
        } deriving (Eq)

instance Show Board where
    show a = show (board a)


-- Constructors

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard r c = Board (newMatrixDefault r c (Cell 0 Free 0)) (r*c)

boardToStrings :: Board -> [String]
boardToStrings b = matrixToStrings (board b) cellToString

prettyBoard :: Board -> String
prettyBoard b = prettyMatrix (board b) prettyCell

prettyBoard2 :: Board -> String
prettyBoard2 b = prettyMatrix (board b) prettyCell2


-- Public functions

setWall :: Coordinates -> Board -> Board
setWall c b = setState c b Wall

setBusy :: Coordinates -> Board -> Board
setBusy c b = setState c b Busy

setFree :: Coordinates -> Board -> Board
setFree c b = setState c b Free

setValue :: Coordinates -> Board -> Int -> Board
setValue c b v = setIntProperty c b updateCellValue v

updateMoves :: Coordinates -> Board -> Int -> Board
updateMoves c b v = setIntProperty c b updateCellMoves v

editMoves :: Coordinates -> Board -> Int -> Board
editMoves c b v = setIntProperty c b editCellMoves v

decreaseMoves :: Coordinates -> Board -> Board
decreaseMoves c b = updateMoves c b (-1)

setWalls :: [Coordinates] -> Board -> Board
setWalls [] b = b
setWalls (x:xs) b = setWalls xs newBoard
                  where newBoard = setWall x b

initValue :: Coordinates -> Board -> Int -> Board
initValue c@(x,y) b v = newBoard
                    where matrix = board b
                          oldCell = getElem matrix x y
                          tempCell = updateCellState oldCell Busy
                          tempCell' = updateCellMoves tempCell 0
                          newCell = updateCellValue tempCell' v
                          tempMatrix = setElem matrix newCell x y
                          tempBoard = Board tempMatrix (squares b)
                          newBoard = decreaseVecinity c tempBoard

getCell :: Coordinates -> Board -> BoardCell
getCell (x,y) b = getElem (board b) x y

getCellState :: Coordinates -> Board -> CellState
getCellState c b = state (getCell c b)

getCellValue :: Coordinates -> Board -> Int
getCellValue c b = value (getCell c b)

getCellMoves :: Coordinates -> Board -> Int
getCellMoves c b = moves (getCell c b)

isCellFree :: Coordinates -> Board -> Bool
isCellFree c b = isFree (getCell c b)

isCellBusy :: Coordinates -> Board -> Bool
isCellBusy c b = isBusy (getCell c b)

isCellWall :: Coordinates -> Board -> Bool
isCellWall c b = isWall (getCell c b)

getRowsCount :: Board -> Int
getRowsCount (Board m _) = rowsCount m

getColsCount :: Board -> Int
getColsCount (Board m _) = colsCount m

getNewCoord :: Coordinates -> Direction -> Coordinates
getNewCoord (c1, c2) dir = (c1 + c1', c2 + c2') 
                           where (c1', c2') = mapDirToTuple dir

checkValidNumberCoord :: Coordinates -> Board -> Bool
checkValidNumberCoord c b = isCorrect && isfree
                              where isCorrect = checkCorrectCoord c b
                                    isfree = isCellFree c b

checkValidCoord :: Coordinates -> Board -> Bool
checkValidCoord c b = isCorrect && (not iswall)
                        where isCorrect = checkCorrectCoord c b
                              iswall = isCellWall c b

checkCorrectCoord :: Coordinates -> Board -> Bool
checkCorrectCoord (x, y) b = positives && insideRange
                              where positives = x >= 0 && y >= 0
                                    matrix = board b
                                    rows = rowsCount matrix
                                    columns = colsCount matrix
                                    insideRange = x < rows && y < columns

getValidInitPos :: Board -> Int -> Coordinates
getValidInitPos brd seed = if (checkValidNumberCoord coord brd) then coord else getValidInitPos brd newSeed
                         where 
                              tup = genRandTuple seed
                              coord = extractPos brd tup
                              newSeed = genRandInt (fst tup)

extractPos :: Board -> (Int, Int) -> Coordinates
extractPos brd (t1, t2) = ((mod t1 x), (mod t2 x))
                         where 
                              x = getRowsCount brd
                              y = getColsCount brd

checkValidDeletion :: Coordinates -> Board -> Bool
checkValidDeletion pos brd = (checkValidCoord pos brd) && val /= 1 && val /= max
                              where
                                   val = getCellValue pos brd
                                   max = squares brd

getMinMoveWarnsdorff :: Board -> Coordinates -> Direction
getMinMoveWarnsdorff b c = getMinMoveWarnsdorff2 b c (None, 9) directions


getRandomMinMove :: Board -> Coordinates -> Int -> Direction
getRandomMinMove brd coord seed = if minDirs == []
                                  then None
                                  else getRandomElementFromList minDirs seed
                              where
                                   minDirs = getAllMinMoves brd coord

getAllMinMoves :: Board -> Coordinates -> [Direction]
getAllMinMoves brd coord = getAllMinMoves2 brd coord ([],9) directions




-- Private Functions

decreaseVecinity :: Coordinates -> Board -> Board
decreaseVecinity c b = decreaseVecinity2 c b directions

decreaseVecinity2 :: Coordinates -> Board -> [Direction] -> Board
decreaseVecinity2 _ b [] = b
decreaseVecinity2 c b (None:xs) = decreaseVecinity2 c b xs
decreaseVecinity2 c b (x:xs) = decreaseVecinity2 c b' xs
                               where c' = getNewCoord c x
                                     b' = decreaseIfValid c' b

decreaseIfValid :: Coordinates -> Board -> Board
decreaseIfValid c b = if (checkValidNumberCoord c b)
                     then decreaseMoves c b
                     else b                 

setIntProperty :: Coordinates -> Board -> (BoardCell -> Int -> BoardCell) -> Int -> Board
setIntProperty (x,y) b f v = Board newMatrix (squares b)
                    where matrix = board b
                          oldCell = getElem matrix x y
                          newCell = f oldCell v
                          newMatrix = setElem matrix newCell x y

setState :: Coordinates -> Board -> CellState -> Board
setState (x,y) b s = Board newMatrix newSquares
                where sq = squares b
                      newSquares = terOp (s == Wall) (sq - 1) sq
                      matrix = board b
                      oldCell = getElem matrix x y
                      newCell = updateCellState oldCell s
                      newMatrix = setElem matrix newCell x y

isState :: Coordinates -> Board -> CellState -> Bool
isState c b stat = stat == cellStat
                    where cellStat = state (getCell c b)

getAllMinMoves2 :: Board -> Coordinates -> ([Direction], Int) -> [Direction] -> [Direction]
getAllMinMoves2 _ _ (dir, _) [] = dir
getAllMinMoves2 b c acc (None:xs) = getAllMinMoves2 b c acc xs
getAllMinMoves2 brd coord acc@(dir,val) (x:xs) = getAllMinMoves2 brd coord acc' xs
                                   where
                                        newCord = getNewCoord coord x
                                        temp = if (checkValidNumberCoord newCord brd)
                                               then getCellMoves newCord brd
                                               else 9
                                        acc' = if (temp < val)
                                               then ([x], temp)
                                               else if (temp == val && temp < 9)
                                                    then ((x:dir), val)
                                                    else acc

getMinMoveWarnsdorff2 :: Board -> Coordinates -> (Direction, Int) -> [Direction] -> Direction
getMinMoveWarnsdorff2 _ _ (dir, _) [] = dir
getMinMoveWarnsdorff2 b c acc (None:xs) = getMinMoveWarnsdorff2 b c acc xs
getMinMoveWarnsdorff2 brd coord acc (x:xs) = getMinMoveWarnsdorff2 brd coord acc' xs
                                   where
                                        newCord = getNewCoord coord x
                                        temp = if (checkValidNumberCoord newCord brd)
                                               then getCellMoves newCord brd
                                               else 9
                                        acc' = terOp (temp < (snd acc)) (x, temp) acc
