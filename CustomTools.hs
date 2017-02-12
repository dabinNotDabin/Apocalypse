{- |
Module      : CustomTools
Description : Custom Data Types and Functions
-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CustomTools where

import ApocTools
import System.Random
import System.IO.Unsafe



-- | Used to indicate the outcome of a move where both players acted.
data MoveType = BlackDodge              -- ^ White attempted to capture but Black moved that piece
              | WhiteDodge              -- ^ Black attempted to capture but White moved that piece
              | Swap                    -- ^ Black and White attempted to capture eachother thus swapped places
              | Clash                   -- ^ Black and White moved into the same square
              | NoEvent                 -- ^ None of the above
                deriving (Eq)

-- | Used to indicate the outcome of a clash
data Outcome  = Win   -- ^ Used to indicate a Win in a Clash  -- Taken from the perspective of some Player
              | Loss  -- ^ Used to indicate a Loss in a Clash -- Taken from the perspective of some Player
              | Tie   -- ^ Used to indicate a Tie in a Clash  -- Taken from the perspective of some Player
                deriving (Eq)



-- | Used to represent options the pieces may have for AI strategy implementation, in alphabetical order to facilitate use of Ord. 
--   TakeKnight will be considered the greatest move where a FreePawn Move will be considered the least.
data PlayOption = AttackKnight       -- ^ Indicates that a Knight Capture is available
                | BlastPawn         -- ^ Indicates that a Pawn Capture is available
                | BoostPawn          -- ^ Indicates that a move to Upgrade a Pawn is available
                | EmptyCell          -- ^ Indicates that a Regular move is available (None of the Above)
                | PlacePawn          -- ^ Indicates that a move to receive a  Pawn Placement turn is available
                  deriving (Eq, Ord)



-- | Triple that contains;
--   A source coordinate and a list of tuples each containing a destination coordinate and it's corresponding PlayOption
--   This can be used to represent a piece on the board, it's location, and a set of moves it may make. 
type MovesForPiece = ((Int, Int) ,  [((Int, Int) , PlayOption)])


-- | A list of 'MovesForPiece'
type MoveListForPlayer = [MovesForPiece]








---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)






pawnUpgradeRequired :: Maybe PlayType -> Bool
pawnUpgradeRequired Nothing = True
pawnUpgradeRequired _       = False



-- | Takes the current 'GameState' and a 'Player' and returns either 'PawnPlacement' or 'Normal' to indicate
--   the expected user input for that 'Player', for that turn or returns 'Nothing' if it's an upgrade round.
determinePlayType :: GameState -> Player -> Maybe PlayType
determinePlayType state Black  = determineBlackPlayType state
determinePlayType state White  = determineWhitePlayType state


-- |Helper function for determinePlayType;
-- If any square in row 0 contains a 'BlackPawn', 'PlayType' is 'PawnPlacement', otherwise it's 'Normal', or returns 'Nothing' if a pawn is to be upgraded
determineBlackPlayType :: GameState -> Maybe PlayType
determineBlackPlayType state
    | (length [(a,0) | a <- [0..4], (getFromBoard (theBoard state) (a,0)) == BP] > 0) &&
                                     getNumKnights (theBoard state) Black > 1         && 
                                    (blackPlay state) /= NullPlacedPawn               &&
                                    (whitePlay state) /= NullPlacedPawn                    = Just PawnPlacement
    | (length [(a,0) | a <- [0..4], (getFromBoard (theBoard state) (a,0)) == BP] > 0) &&
                                     getNumKnights (theBoard state) Black < 2              = Nothing
    | otherwise                                                                            = Just Normal


-- |Helper function for determinePlayType;
-- If any square in row 4 contains a 'WhitePawn', 'PlayType' is 'PawnPlacement', otherwise it's 'Normal', or returns 'Nothing' if a pawn is to be upgraded
determineWhitePlayType :: GameState -> Maybe PlayType
determineWhitePlayType state
    | (length [(a,4) | a <- [0..4], (getFromBoard (theBoard state) (a,4)) == WP] > 0) &&
                                     getNumKnights (theBoard state) White > 1              = Just PawnPlacement
    | (length [(a,0) | a <- [0..4], (getFromBoard (theBoard state) (a,4)) == WP] > 0) &&
                                     getNumKnights (theBoard state) White < 2              = Nothing
    | otherwise                                                                            = Just Normal



-- | Takes a 'Board' and a 'Player' and returns the number of Knights that player has
getNumKnights :: Board -> Player -> Int
getNumKnights board Black     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == BK]
getNumKnights board White     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == WK]

-- | Takes a 'Board' and a 'Player' and returns the number of Pawns that player has
getNumPawns :: Board -> Player -> Int
getNumPawns board Black     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == BP]
getNumPawns board White     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == WP]








-- | Returns the coordinate (if any) in a singleton list that an upgradable pawn exists for the specified 'Player'
--   Length can be zero -- indicates no such pawn exists
getUpgradablePawnLocation :: Board -> Player -> [(Int, Int)]
getUpgradablePawnLocation board Black     = [(a,0) | a <- [0..4], (getFromBoard (board) (a,0)) == BP]
getUpgradablePawnLocation board White     = [(a,4) | a <- [0..4], (getFromBoard (board) (a,4)) == WP]





-- | Checks whether either 'Player' has reached 0 pawns
--   If zero pawns are found, returns the winner ('Black' or 'White')
--   Returns 'Nothing' for no winner
checkForZeroPawns :: Board -> Maybe Player
checkForZeroPawns board
    | ((getNumPawns board Black) == 0)    = Just White
    | ((getNumPawns board White) == 0)    = Just Black
    | otherwise                           = Nothing



-- | Checks whether the specified Player has accumulated 2 penalty points, returns true if they have
checkPenalties :: GameState -> Player -> Bool
checkPenalties state Black = ((blackPen state) >= 2)
checkPenalties state White = ((whitePen state) >= 2)




 



upgradePawns :: GameState -> Bool -> Bool -> GameState --board, white, black
upgradePawns oldGS white black 
        | (white == True  && black == True)  = updateBoth  oldGS
        | (white == True  && black == False) = updateWhite oldGS
        | (white == False && black == True)  = updateBlack oldGS
        | otherwise                          = oldGS

updateBoth :: GameState -> GameState
updateBoth g = updateWhite (updateBlack g)

updateBlack :: GameState -> GameState
updateBlack g = 
        let board = (theBoard g)
            coord = getUpgradablePawnLocation board Black
        in  GameState (UpgradedPawn2Knight (head coord))
                      ((blackPen  g))
                      (None)
                      ((whitePen  g))
                      ((replace2 board (head coord) BK))

updateWhite :: GameState -> GameState
updateWhite g = 
        let board = (theBoard g)
            coord = getUpgradablePawnLocation board White
        in  GameState (None)
                      ((blackPen  g))
                      (UpgradedPawn2Knight (head coord))
                      ((whitePen  g))
                      ((replace2 board (head coord) WK))

















-- ========================================================================================
-- ========================Functions For Use In Implementing AI============================
-- ========================================================================================


-- | 
getEmptyCells :: Board -> (Int, Int)
getEmptyCells board = let emptyCells = [(a,b) | a <- [0..4], b <- [0..4], (getFromBoard board (a,b) == E)]
                       in  emptyCells !! (unsafePerformIO (randomRIO (0, ((length emptyCells) - 1))))


-- | Gets a list of coordinates that represent the locations of any Pieces on the board of the specified Player
getPieceLocations :: Board -> Player -> [(Int,Int)]
getPieceLocations board Black = [(a,b) | a <- [0..4], b <- [0..4], (getFromBoard board (a,b)) == BK || (getFromBoard board (a,b)) == BP]
getPieceLocations board White = [(a,b) | a <- [0..4], b <- [0..4], (getFromBoard board (a,b)) == WK || (getFromBoard board (a,b)) == WP]



-- | Gets a list of coordinates that represent the valid moves for a Knight (whose location is the 3rd argument)
--   Assumes the board is empty
getKnightMoves :: Board -> Player -> (Int, Int) -> [(Int, Int)]
getKnightMoves board Black (x,y) = 
    [(a,b) | a <- [0..4], b <- [0..4], ((getFromBoard board (a,b) == E)  ||  
                                       (getFromBoard board (a,b) == WP)  || 
                                       (getFromBoard board (a,b) == WK))  &&
                                       ((abs (x - a) == 1  &&  abs (y - b) == 2)   ||  (abs (x - a) == 2  &&  abs (y - b) == 1))   ]  
getKnightMoves board White (x,y) = 
    [(a,b) | a <- [0..4], b <- [0..4], ((getFromBoard board (a,b) == E)  ||  
                                       (getFromBoard board (a,b) == BP)  || 
                                       (getFromBoard board (a,b) == BK))  &&
                                       ((abs (x - a) == 1  &&  abs (y - b) == 2)   ||  (abs (x - a) == 2  &&  abs (y - b) == 1))   ]  


-- | Gets a list of coordinates that represent the valid moves for a Pawn (whose location is the 3rd argument)
--   Checks for capture possibilities to represent all diagonal move choices as well as forward only moves
getPawnMoves :: Board -> Player -> (Int, Int) -> [(Int, Int)]
getPawnMoves board Black (x,y) = 
    [(a,b) | a <- [0..4], b <- [0..4], ((getFromBoard board (a,b) == WK || getFromBoard board (a,b) == WP) &&
                                       (abs (x - a) == 1) && ((y - b) == 1))                               ||
                                       ((getFromBoard board (a,b) == E) && ((x - a) == 0) && ((y - b) == 1))  ] 
getPawnMoves board White (x,y) = 
    [(a,b) | a <- [0..4], b <- [0..4], ((getFromBoard board (a,b) == BK || getFromBoard board (a,b) == BP) &&
                                       (abs (x - a) == 1) && ((b - y) == 1))                               ||
                                       ((getFromBoard board (a,b) == E) && ((x - a) == 0) && ((b - y) == 1))  ] 





-- | Takes;
--      A List of coordinates (that may represent valid move choices for some Player).
--      A list of coordinates (that may represent opposing piece locations).
--      Returns the matching coordinates from the first two lists.
--      Note: There are no restrictions on the meaning of the lists that are passed in.
getCaptureLocations :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
getCaptureLocations [] []     = Nothing
getCaptureLocations []  _     = Nothing
getCaptureLocations  _ []     = Nothing
getCaptureLocations xs ys     = Just [(a,b) | a <- [0..4], b <- [0..4], (elem (a,b) xs) && (elem (a,b) ys)]













-- ========================================================================================
-- =========================Functions Used to Find Available Moves=========================
-- ========================================================================================


-- | Gets the PlayOption for one specific move where the source is the first arg and the destination is the second arg
-- | Doesn't validate moves, assumes the move to be valid for the piece at the source coordinate
getPlayOption :: Board -> Player -> (Int, Int) -> (Int, Int) -> PlayOption
getPlayOption board Black (w,x) (y,z)
    | dest   == WP                                                                     = BlastPawn      
    | dest   == WK                                                                     = AttackKnight    
    | source == BP  &&  dest == E  &&   row == 0  &&  getNumKnights board Black == 2   = PlacePawn
    | source == BP  &&  dest == E  &&   row == 0  &&  getNumKnights board Black <  2   = BoostPawn
    | otherwise                                                                        = EmptyCell
    where source = getFromBoard board (w,x)
          dest   = getFromBoard board (y,z)
          row    = z
getPlayOption board White (w,x) (y,z)
    | dest == BP                                                                       = BlastPawn        
    | dest == BK                                                                       = AttackKnight    
    | source == WP  &&  dest == E  &&   row == 4  && getNumKnights board White == 2    = PlacePawn
    | source == WP  &&  dest == E  &&   row == 4  && getNumKnights board White <  2    = BoostPawn
    | otherwise                                                                        = EmptyCell
    where source = getFromBoard board (w,x)
          dest   = getFromBoard board (y,z)
          row    = z




-- | Returns a 'MovesForPiece' tuple that holds information about all available moves for the specified piece
--   Takes a Board, a source coordinate and a Player
--      The source coordinate is identified as a Piece and all valid moves for that piece are 
populateMoveList :: Board -> (Int, Int) -> Player -> MovesForPiece
populateMoveList board (x,y) colour
    | source == BP  || source == WP    =  ((x,y)  ,  getAllPlayOptions board (x,y) colour pawnMoves)
    | source == BK  || source == WK    =  ((x,y)  ,  getAllPlayOptions board (x,y) colour knightMoves)
    where pawnMoves   = getPawnMoves   board colour (x,y)
          knightMoves = getKnightMoves board colour (x,y)
          source      = getFromBoard   board (x,y)





-- | Returns a list of tuples where each pair contains a coordinate and a 'PlayOption' for that coordinate.
--   Takes;
--     A Board, a source coordinate, a Player and a list of destination coordinates
--       For each destination, it determines an associated 'PlayOption' and adds that pair to the list
getAllPlayOptions :: Board -> (Int, Int) -> Player -> [(Int,Int)] -> [((Int,Int) , PlayOption)]
getAllPlayOptions _      _    _       []   = []
getAllPlayOptions board (x,y) Black (z:zs) = [(z, (getPlayOption board Black (x,y) z))] ++ getAllPlayOptions board (x,y) Black zs 
getAllPlayOptions board (x,y) White (z:zs) = [(z, (getPlayOption board White (x,y) z))] ++ getAllPlayOptions board (x,y) White zs 





-- | Gets a [MovesForPiece] where each element is a 'MovesForPiece' representing the moves available for each
--     piece indicated in the list provided as the second argument.
getAllPlaysForPlayer :: Board -> Maybe [(Int, Int)] -- ^ Expected to be a list of coordinates representing the locations of all pieces owned by the specified Player
                     -> Player -> MoveListForPlayer -- ^ A list of all possible moves (tuples) 'MovesForPiece' for the Player specified.
getAllPlaysForPlayer _     (Just  [])  colour = []
getAllPlaysForPlayer board (Just (x:xs)) colour = [(populateMoveList board x colour)] ++ (getAllPlaysForPlayer board (Just xs) colour)



randomN :: Int -> IO(Int)
randomN max = do 
    g <- newStdGen
    let number = (take max (randomRs (0, max) g))
     in  return (number !! (randomNumRange (max :: Int)))



randomNum :: Int
randomNum = (unsafePerformIO (randomRIO (0, 10))) :: Int       

randomNumRange :: Int -> Int
randomNumRange max = (unsafePerformIO (randomRIO (0, (max-1)))) :: Int       

getRand :: [(Int, Int)] -> [(Int,Int)] -> [(Int,Int)]
getRand sources dests = 
    let rand = randomNumRange (length sources)
     in [sources !! rand, dests !! rand]


getBestofGreedyMoves :: [((Int,Int) , (Int,Int) , PlayOption)] -> IO (Maybe [(Int,Int)])
getBestofGreedyMoves  []                        = return (Just [(4,4), (2,3)])
getBestofGreedyMoves [(x,y,z)]                  = return (Just [x,y])
getBestofGreedyMoves  zs@(x:xs)                 = do
    number <- randomN ((length zs) - 1)
    let source = [ src | src <- (map first zs), dst <- (map second zs), x <- (map thrd zs),
                     x == minimum (map thrd zs) || x > EmptyCell, elem (src, dst, x) zs ]
        dest   = [ dst | src <- (map first zs), dst <- (map second zs), x <- (map thrd zs), 
                     x == minimum (map thrd zs) || x > EmptyCell, elem (src, dst, x) zs ]
     in (if number > 3 then getBestofGreedyMoves zs
         else return (Just [ first (zs !! number), second (zs !! number) ]))
        

{-
getBestofGreedyMoves  zs@(x:xs)
    | number > 5              = getBestofGreedyMoves (randomReorder (everySecond xs))
    | otherwise               = getBestofGreedyMoves (randomReorder (everySecond zs))
    where source = [ src | src <- (map first zs), dst <- (map second zs), x <- (map thrd zs),
                     x == minimum (map thrd zs) || x > EmptyCell, elem (src, dst, x) zs ]
          dest   = [ dst | src <- (map first zs), dst <- (map second zs), x <- (map thrd zs), 
                     x == minimum (map thrd zs) || x > EmptyCell, elem (src, dst, x) zs ]
          number <- randomN (length zs)
                      
-}

getMovesGreedy :: MoveListForPlayer -> [Maybe ((Int,Int) , (Int,Int) , PlayOption)]
getMovesGreedy  []               = []
getMovesGreedy (x:xs)            = [getMoveGreedy' x] ++ getMovesGreedy xs




getMoveGreedy' :: MovesForPiece -> Maybe ((Int,Int) , (Int,Int) , PlayOption)
getMoveGreedy' (src, [])           = Nothing              
getMoveGreedy' (src, zs)           = let bestOption = getPiecesBestOption zs
                                      in Just (src,  (fst bestOption) , (snd bestOption))  
  


getPiecesBestOption :: [((Int, Int), PlayOption)] -> ((Int, Int), PlayOption)
getPiecesBestOption (z:[])             = z 
getPiecesBestOption  zs                = getBestOptionFromList zs  (minimum (map snd zs))



getBestOptionFromList :: [((Int, Int), PlayOption)] -> PlayOption -> ((Int, Int), PlayOption)
getBestOptionFromList (x:[]) option = x
getBestOptionFromList (x:xs) option
    | (snd x == option && randomNum == 10 && length xs /= 0)  = getBestOptionFromList xs (minimum (map snd xs))
    |  snd x == option                                        = x
    |  otherwise                                              = getBestOptionFromList xs option






filterNothingMoves :: [Maybe ((Int,Int) , (Int,Int) , PlayOption)] -> [((Int,Int) , (Int,Int) , PlayOption)]
filterNothingMoves  []             = []
filterNothingMoves  [Nothing]      = []
filterNothingMoves  (Nothing:xs)   = filterNothingMoves xs 
filterNothingMoves  [Just x]       = [x]
filterNothingMoves  ((Just x):xs)  = [x] ++ filterNothingMoves xs





randomReorder :: [((Int,Int) , (Int,Int) , PlayOption)] -> [((Int,Int) , (Int,Int) , PlayOption)]
randomReorder []      = []
randomReorder [x]     = [x]
randomReorder [x,y]   = [y,x]
randomReorder (x:xs)  = 
    let rand = randomNumRange (length xs)
     in (take rand xs) ++ [x]

{-
  let playsForPlayer = getAllPlaysForPlayer (theBoard state) (Just (getPieceLocations (theBoard state) colour)) colour
     in return (getBestofGreedyMoves (filterNothingMoves (getMovesGreedy playsForPlayer)))

-}








-- ========================================================================================
-- ===========================Functions Regarding Tuples===================================
-- ========================================================================================



thrd :: ((Int,Int) , (Int,Int) , PlayOption) -> PlayOption
thrd (_, _, x) = x


first :: ((Int,Int) , (Int,Int) , PlayOption) -> (Int, Int)
first ((a,b), _, _) = (a,b)



second :: ((Int,Int) , (Int,Int) , PlayOption) -> (Int, Int)
second (_, (c,d), _) = (c,d)




everySecond :: [((Int,Int) , (Int,Int) , PlayOption)] -> [((Int,Int) , (Int,Int) , PlayOption)]
everySecond []     = []
everySecond [x]    = [x]
everySecond [x,y]  = [y] 
everySecond (x:xs) = [head xs] ++ everySecond xs 



-- ========================================================================================
-- ==================================Instances of Show=====================================
-- ========================================================================================



instance Show MoveType where
         show BlackDodge = "Black Dodge"
         show WhiteDodge = "White Dodge"
         show Swap       = "Swap"
         show Clash      = "Clash"
         show NoEvent    = "No Event"




instance Show Outcome where
         show Win  = "Win"
         show Loss = "Loss"
         show Tie  = "Tie"




instance Show PlayType where
         show Normal        = "Normal"
         show PawnPlacement = "PawnPlacement"

 



instance Show PlayOption where
         show BlastPawn      = "Capture Pawn"
         show AttackKnight   = "Capture Knight"
         show BoostPawn      = "Upgrade"
         show PlacePawn      = "Place Pawn"
         show EmptyCell      = "Regular"




testBoard       :: GameState
testBoard       = GameState Init 0 Init 0
                  [ [WK, WP, E, WP, WK],
                    [WP, E , E , E , WP],
                    [E , E , E , E , E ],
                    [BP, E , E , E , BP],
                    [BK, BP, WP, BP, BK] ]




testBoard2       :: GameState
testBoard2       = GameState Init 0 Init 0
                  [ [WK, WP, E, WP, WK],
                    [WP, E , BP , E , WP],
                    [E , E , E , E , E ],
                    [BP, E , E , E , BP],
                    [BK, BP, BP, BP, BK] ]





testBoard3       :: GameState
testBoard3       = GameState Init 0 Init 0
                  [ [BK, WP, E, WP, WK],
                    [WP, E , WP , E , WP],
                    [E , E , E , E , E ],
                    [BP, E , E , E , BP],
                    [BK, BP, BP, BP, BK] ]




testBoard4       :: GameState
testBoard4       = GameState Init 0 Init 0
                  [ [WK, WP, E, WP, WK],
                    [WP, E , E , E , WP],
                    [E , E , E , E , E ],
                    [E, E , E , E , E],
                    [E, E, E, E, E] ]





testBoard5       :: GameState
testBoard5       = GameState Init 0 Init 0
                  [ [WK, E, E, E, E],
                    [E, E , E , E , WP],
                    [E , WK , E , E , E ],
                    [E, E , E , E , E],
                    [BK, E, E, E, E] ]





testBoard6       :: GameState
testBoard6       = GameState Init 0 Init 0
                  [ [WK, E, E, E, E],
                    [E, E , E , E , WP],
                    [E , WK , E , E , E ],
                    [E, BP , E , E , E],
                    [E, E, E, E, E] ]







testBoard7       :: GameState
testBoard7       = GameState Init 0 Init 0
                  [ [WK, WP, BP, WP, WK],
                    [WP, E , E , E , WP],
                    [E , E , E , E , E ],
                    [BP, E , E , E , BP],
                    [BK, E, BP, BP, BK] ]

















































