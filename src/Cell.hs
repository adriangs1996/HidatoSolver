module Cell where

data BoardCell = Cell{ 
                      value :: Int 
                    , state :: CellState
                    , moves :: Int
                    } deriving(Eq)

instance Show BoardCell where
    show x = "[" ++ (getCorrectNumberFormat (value x) 3) ++ " : " ++ show (state x) ++ " : " ++ show (moves x) ++ "]"


data CellState = Busy | Wall | Free deriving(Show, Eq)

getCorrectNumberFormat :: Int -> Int -> String
getCorrectNumberFormat n maxdigits = concat (replicate x " ") ++ show (n)
                                     where x = maxdigits - (floor (logBase 10 (fromIntegral n)) + 1)

isFree :: BoardCell -> Bool
isFree (Cell _ Free _) = True
isFree _ = False

isBusy :: BoardCell -> Bool
isBusy (Cell _ Busy _) = True
isBusy _ = False

isWall :: BoardCell -> Bool
isWall (Cell _ Wall _) = True
isWall _ = False


updateCellValue :: BoardCell -> Int -> BoardCell 
updateCellValue (Cell _ s m) val = Cell val s m 

updateCellState :: BoardCell -> CellState -> BoardCell
updateCellState (Cell _ _ m) Free = Cell 0 Free m
updateCellState (Cell v _ m) stat = Cell v stat m

decreaseCellMoves :: BoardCell -> BoardCell
decreaseCellMoves cell = updateCellMoves cell (-1)

updateCellMoves :: BoardCell -> Int -> BoardCell
updateCellMoves c@(Cell v s m) count = editCellMoves c m'
                                where m' = m + count

editCellMoves :: BoardCell -> Int -> BoardCell
editCellMoves (Cell v s _) val = Cell v s val  

prettyCell :: BoardCell -> String
prettyCell (Cell val stat _) = if stat == Wall
                               then "  X"
                               else if val == 0
                                     then "  -"
                                     else getCorrectNumberFormat val 3


prettyCell2 :: BoardCell -> String
prettyCell2 (Cell val stat _) = if stat == Wall
                                then "   "
                                else if val == 0
                                      then "  -"
                                      else getCorrectNumberFormat val 3

cellToString :: BoardCell -> String
cellToString (Cell val stat _) = if stat == Wall
                               then "  ."
                               else getCorrectNumberFormat val 3