module HidatoBoard where
import Solve
import Board (Board, boardToStrings)
import Utils


-- Esta es la función que se debe usar, el randSeed se le pasa como parámetro desde donde se llame e.g: el método main
generateBoard :: Int -> [String]
generateBoard randSeed = boardToStrings (makeHidatoBoard brd randSeed)
                        where brd = selectBoard (genRandInt randSeed)

selectBoard :: Int -> Board
selectBoard rand = getRandomElementFromList boardTemplates rand

