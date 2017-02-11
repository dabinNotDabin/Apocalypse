{- |
Module      : CustomTools
Description : Custom Data Types and Functions
-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CustomTools where

import ApocTools




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
                | AttackPawn         -- ^ Indicates that a Pawn Capture is available
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
                                     getNumKnights (theBoard state) Black > 1              = Just PawnPlacement
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





















-- ========================================================================================
-- ========================Functions For Use In Implementing AI============================
-- ========================================================================================


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
    | dest   == WP                                                                     = AttackPawn      
    | dest   == WK                                                                     = AttackKnight    
    | source == BP  &&  dest == E  &&   row == 0  &&  getNumKnights board Black == 2   = PlacePawn
    | source == BP  &&  dest == E  &&   row == 0  &&  getNumKnights board Black <  2   = BoostPawn
    | otherwise                                                                        = EmptyCell
    where source = getFromBoard board (w,x)
          dest   = getFromBoard board (y,z)
          row    = z
getPlayOption board White (w,x) (y,z)
    | dest == BP                                                                       = AttackPawn        
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







getMoveGreedy' :: MovesForPiece -> Maybe ((Int,Int) , (Int,Int) , PlayOption)
getMoveGreedy' (src, [])           = Nothing              
getMoveGreedy' (src, zs)           = let bestOption = getPiecesBestOption zs
                                      in Just (src,  (fst bestOption) , (snd bestOption))  
  


getPiecesBestOption :: [((Int, Int), PlayOption)] -> ((Int, Int), PlayOption)
getPiecesBestOption (z:[])             = z 
getPiecesBestOption  zs                = getBestOptionFromList zs  (minimum (map snd zs))



getBestOptionFromList :: [((Int, Int), PlayOption)] -> PlayOption -> ((Int, Int), PlayOption)
getBestOptionFromList (x:xs) option 
    | snd x == option           = x
    | otherwise                 = getBestOptionFromList xs option




getMoveGreedy :: MoveListForPlayer -> [Maybe ((Int,Int) , (Int,Int) , PlayOption)]
getMoveGreedy  []               = []
getMoveGreedy (x:xs)            = [getMoveGreedy' x] ++ getMoveGreedy xs






getBestofGreedyMoves :: [((Int,Int) , (Int,Int) , PlayOption)] -> Maybe [(Int,Int)]
getBestofGreedyMoves  []                        = Nothing
getBestofGreedyMoves [(x,y,z)]                  = Just [x,y]
getBestofGreedyMoves  zs                  
    = Just [source , dest] 
      where source = [ src | src <- (map first zs), dst <- (map second zs), x <- (map thrd zs), x == minimum (map thrd zs), elem (src, dst, x) zs ] !! 0
            dest   = [ dst | src <- (map first zs), dst <- (map second zs), x <- (map thrd zs), x == minimum (map thrd zs), elem (src, dst, x) zs ] !! 0



-- | Removes any set of 'MovesForPiece' that equals Nothing
filterNothingMoves :: [Maybe ((Int,Int) , (Int,Int) , PlayOption) ] -> [((Int,Int) , (Int,Int) , PlayOption)]
filterNothingMoves  []             = []
filterNothingMoves  [Just x]       = [x]
filterNothingMoves  ((Just x):xs)  = [x] ++ filterNothingMoves xs













-- ========================================================================================
-- ===========================Functions Regarding Tuples===================================
-- ========================================================================================


-- | Gets the third element of a triple (with a very specific type pattern)
thrd :: ((Int,Int) , (Int,Int) , PlayOption) -> PlayOption
thrd (_, _, x) = x

-- | Gets the second element of a triple (with a very specific type pattern)
first :: ((Int,Int) , (Int,Int) , PlayOption) -> (Int, Int)
first ((a,b), _, _) = (a,b)


-- | Gets the first element of a triple (with a very specific type pattern)
second :: ((Int,Int) , (Int,Int) , PlayOption) -> (Int, Int)
second (_, (c,d), _) = (c,d)










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




instance Show PlayOption where
         show AttackPawn     = "Capture Pawn"
         show AttackKnight   = "Capture Knight"
         show BoostPawn      = "Upgrade"
         show PlacePawn      = "Place Pawn"
         show EmptyCell      = "Regular"




testBoard       :: GameState
testBoard       = GameState Init 0 Init 0
                  [ [E, WP, WP, WP, WK],
                    [WP, E , E , E , WP],
                    [E , E , WK , E , E ],
                    [BP, E , E , E , BP],
                    [BK, BP, BP, BP, BK] ]




testBoard2       :: GameState
testBoard2       = GameState Init 0 Init 0
                  [ [WK, WP, E, WP, WK],
                    [WP, E , BP , E , WP],
                    [E , E , E , E , E ],
                    [BP, E , E , E , BP],
                    [BK, BP, BP, BP, BK] ]





testBoard3       :: GameState
testBoard3       = GameState Init 0 Init 0
                  [ [WK, WP, BP, WP, WK],
                    [WP, E , BP , E , WP],
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















































