{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman where

import ApocTools
import CustomTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human    :: Chooser
human state playType colour = do
    promptUser playType colour
    input <- getLine

--TODO 3) Done by Rob

    return $ inputToCoordinates ([read [x] :: Int | x <- input, elem x ['0'..'4']])




greedy    :: Chooser
greedy state Normal colour = 
    let playsForPlayer = getAllPlaysForPlayer (theBoard state) (Just (getPieceLocations (theBoard state) colour)) colour
     in return (getBestofGreedyMoves (filterNothingMoves (getMoveGreedy playsForPlayer)))













greedy b PawnPlacement c = return (Just [(2,2)])



evasive   :: Chooser
evasive b Normal        c = return (Just [(4,4),(3,2)])
evasive b PawnPlacement c = return (Just [(2,2)])
















-- ========================================================================================
-- =============================Functions That Might be Useful=============================
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




filterNothingMoves :: [Maybe ((Int,Int) , (Int,Int) , PlayOption) ] -> [((Int,Int) , (Int,Int) , PlayOption)]
filterNothingMoves  []             = []
filterNothingMoves  [Just x]       = [x]
filterNothingMoves  ((Just x):xs)  = [x] ++ filterNothingMoves xs






thrd :: ((Int,Int) , (Int,Int) , PlayOption) -> PlayOption
thrd (_, _, x) = x


first :: ((Int,Int) , (Int,Int) , PlayOption) -> (Int, Int)
first ((a,b), _, _) = (a,b)



second :: ((Int,Int) , (Int,Int) , PlayOption) -> (Int, Int)
second (_, (c,d), _) = (c,d)


