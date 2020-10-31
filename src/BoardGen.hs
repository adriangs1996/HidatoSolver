module BoardGen (
    createNewBoard,
    createNewBoardFromConfig
)
where
import Board
import Utils
import MyMatrix

-- Types

type BoardConfig = (Int, Int, [Coordinates])

-- Public functions

createNewBoard :: Int -> Int -> [Coordinates] -> Board
createNewBoard x y c = initMoves b
                       where b' = createEmptyBoard x y
                             b = createWalls b' c 

createNewBoardFromConfig :: BoardConfig -> Board
createNewBoardFromConfig (x, y, c) = createNewBoard x y c


-- Private functions

createWalls :: Board -> [Coordinates] -> Board
createWalls b c = setWalls c b

initMoves :: Board -> Board
initMoves b = initMoves2 b coordinates
              where rows = rowsCount $ board b
                    cols = colsCount $ board b
                    coordinates = [(i, j) | i <- [0..rows], j <- [0..cols]]

initMoves2 :: Board -> [Coordinates] -> Board
initMoves2 b [] = b
initMoves2 b (x:xs) = initMoves2 b' xs
                      where b' = if checkValidCoord x b
                                 then addMoves b x
                                 else b

addMoves :: Board -> Coordinates -> Board
addMoves b c = editMoves c b count
               where count = calculateMoves b c directions 0

calculateMoves :: Board -> Coordinates -> [Direction] -> Int -> Int 
calculateMoves _ _ [] count = count
calculateMoves b coord (None:xs) count = calculateMoves b coord xs count
calculateMoves b coord (x:xs) count = calculateMoves b coord xs count'
                                    where newCoord = getNewCoord coord x
                                          count' = if checkValidCoord newCoord b
                                                   then count + 1
                                                   else count