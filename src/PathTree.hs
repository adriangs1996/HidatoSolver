module PathTree where
--import MyMatrix
--import Cell
import Utils

data PathTree a = Empty  
                | Ptree {
                  value :: a
                , pathCount :: Int
                , pos :: (Int, Int)
                , childUp :: PathTree a
                , childDown :: PathTree a
                , childLeft :: PathTree a
                , childRight :: PathTree a
                , childLftDn :: PathTree a
                , childLftUp :: PathTree a
                , childRgtDn :: PathTree a
                , childRgtUp :: PathTree a
                } deriving(Eq)

instance (Show a, Eq a) => Show (PathTree a) where
  show tree = if isEmpty tree
              then "Empty"
              else prettyPathTree tree 

prettyPathTree :: (Eq a, Show a) => PathTree a -> String
prettyPathTree tree = "[ " ++ prettyValue (value tree) ++ " : " ++ prettyValue (pathCount tree) ++ " : " ++ prettyCoords (pos tree) ++ printChildren tree ++ " ] "

prettyValue :: (Eq a, Show a) => a -> String
prettyValue a = show a

prettyCoords :: Coordinates -> String
prettyCoords (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

printChildren :: (Eq a, Show a) => PathTree a -> String
printChildren tree = prettyChildren tree directions

prettyChildren :: (Eq a, Show a) => PathTree a -> [Direction] -> String
prettyChildren _ [] = ""
prettyChildren tree (None:xs) = prettyChildren tree xs
prettyChildren tree (x:xs) = checkChildShowing child tree dir ++ prettyChildren tree xs
                             where child = mapDirToChild x
                                   dir = shortDirShow x

checkChildShowing :: (Eq a, Show a) => (PathTree a -> PathTree a) -> PathTree a -> String -> String
checkChildShowing func tree dir = if isEmpty (func tree)
                                  then ""
                                  else "\n -> " ++ dir ++ ": " ++ (prettyPathTree (func tree))

mapDirToChild :: Direction -> (PathTree a -> PathTree a)
mapDirToChild North = childUp
mapDirToChild South = childDown
mapDirToChild West = childLeft
mapDirToChild East = childRight
mapDirToChild SouthWest = childLftDn
mapDirToChild NorthWest = childLftUp
mapDirToChild SouthEast = childRgtDn
mapDirToChild NorthEast = childRgtUp


emptyTree :: a -> Int -> Coordinates -> PathTree a
emptyTree x y coord = Ptree x y coord Empty Empty Empty Empty Empty Empty Empty Empty


isEmpty :: (Eq a) => PathTree a -> Bool
isEmpty tree
            | tree == Empty = True
            | otherwise     = False

isEmptyTree :: PathTree a -> Bool
isEmptyTree (Ptree _ _ _ Empty Empty Empty Empty Empty Empty Empty Empty) = True
isEmptyTree _ = False


insertChild :: PathTree a -> Direction -> PathTree a -> PathTree a
insertChild tree North child = insertNorth tree child
insertChild tree South child = insertSouth tree child
insertChild tree West child = insertWest tree child
insertChild tree East child = insertEast tree child
insertChild tree NorthWest child = insertNorWest tree child
insertChild tree SouthWest child = insertSouWest tree child
insertChild tree NorthEast child = insertNorEast tree child
insertChild tree SouthEast child = insertSouEast tree child


insertNorth :: PathTree a -> PathTree a -> PathTree a
insertNorth tree child = Ptree tval tcount tpos child dCH lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            dCH = childDown tree
                            lCh = childLeft tree
                            rCH = childRight tree
                            ldCH = childLftDn tree
                            luCh = childLftUp tree
                            rdCh = childRgtDn tree
                            ruCH = childRgtUp tree

insertSouth :: PathTree a -> PathTree a -> PathTree a
insertSouth tree child = Ptree tval tcount tpos uCh child lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            uCh = childUp tree
                            lCh = childLeft tree
                            rCH = childRight tree
                            ldCH = childLftDn tree
                            luCh = childLftUp tree
                            rdCh = childRgtDn tree
                            ruCH = childRgtUp tree

insertEast :: PathTree a -> PathTree a -> PathTree a
insertEast tree child = Ptree tval tcount tpos uCh dCH lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            uCh = childUp tree
                            dCH = childDown tree
                            lCh = childLeft tree
                            rCH = child
                            ldCH = childLftDn tree
                            luCh = childLftUp tree
                            rdCh = childRgtDn tree
                            ruCH = childRgtUp tree

insertWest :: PathTree a -> PathTree a -> PathTree a
insertWest tree child = Ptree tval tcount tpos uCh dCH lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            uCh = childUp tree
                            dCH = childDown tree
                            lCh = child
                            rCH = childRight tree
                            ldCH = childLftDn tree
                            luCh = childLftUp tree
                            rdCh = childRgtDn tree
                            ruCH = childRgtUp tree

insertNorWest :: PathTree a -> PathTree a -> PathTree a
insertNorWest tree child = Ptree tval tcount tpos uCh dCH lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            uCh = childUp tree
                            dCH = childDown tree
                            lCh = childLeft tree
                            rCH = childRight tree
                            ldCH = childLftDn tree
                            luCh = child
                            rdCh = childRgtDn tree
                            ruCH = childRgtUp tree

insertSouWest :: PathTree a -> PathTree a -> PathTree a
insertSouWest tree child = Ptree tval tcount tpos uCh dCH lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            uCh = childUp tree
                            dCH = childDown tree
                            lCh = childLeft tree
                            rCH = childRight tree
                            ldCH = child
                            luCh = childLftUp tree
                            rdCh = childRgtDn tree
                            ruCH = childRgtUp tree

insertNorEast :: PathTree a -> PathTree a -> PathTree a
insertNorEast tree child = Ptree tval tcount tpos uCh dCH lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            uCh = childUp tree
                            dCH = childDown tree
                            lCh = childLeft tree
                            rCH = childRight tree
                            ldCH = childLftDn tree
                            luCh = childLftUp tree
                            rdCh = childRgtDn tree
                            ruCH = child

insertSouEast :: PathTree a -> PathTree a -> PathTree a
insertSouEast tree child = Ptree tval tcount tpos uCh dCH lCh rCH ldCH luCh rdCh ruCH
                      where tval = value tree
                            tcount = pathCount tree
                            tpos = pos tree
                            uCh = childUp tree
                            dCH = childDown tree
                            lCh = childLeft tree
                            rCH = childRight tree
                            ldCH = childLftDn tree
                            luCh = childLftUp tree
                            rdCh = child
                            ruCH = childRgtUp tree

treeToPosList :: (Eq a) => PathTree a -> [Coordinates]
treeToPosList tree = treeToPosList' tree []

treeToPosList' :: (Eq a) => PathTree a -> [Coordinates] -> [Coordinates]
treeToPosList' Empty list = list
treeToPosList' tree list = treeToPosList' next list'
                  where 
                        posi = pos tree
                        list' = list ++ [posi]
                        next = getNotEmptyChild tree

getNotEmptyChild :: (Eq a) => PathTree a -> PathTree a
getNotEmptyChild Empty = Empty
getNotEmptyChild (Ptree _ _ _ c1 c2 c3 c4 c5 c6 c7 c8)
                                                      |c1 /= Empty = c1
                                                      |c2 /= Empty = c2
                                                      |c3 /= Empty = c3
                                                      |c4 /= Empty = c4
                                                      |c5 /= Empty = c5
                                                      |c6 /= Empty = c6
                                                      |c7 /= Empty = c7
                                                      |c8 /= Empty = c8
                                                      |otherwise = Empty

checkValidTree :: PathTree a -> Bool
checkValidTree Empty = False
checkValidTree tree = (pathCount tree) > 0
