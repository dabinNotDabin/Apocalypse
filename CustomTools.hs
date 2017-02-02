{- |
Module      : CustomTools
Description : Custom Data Types and Functions
-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CustomTools (
    MoveType (BlackDodge, WhiteDodge, Clash, Swap, NoEvent),
    Outcome (Win, Loss, Tie),
    determinePlayType,
    determineBlackPlayType,
    determineWhitePlayType,
    getNumKnights,
    getNumPawns
) where

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

 
 
-- | Used to link strategies (Chooser type) with a String for their name.
--   Useful in printing the outcome of a round.
type Strategies = [(Chooser, String)]



data StrategyList = StrategyList { strategyList :: Strategies }



-- | Takes the current GameState and a Player and returns either PawnPlacement or Normal to indicate
--   the expected user input for that Player, for that turn.
determinePlayType :: GameState -> Player -> Maybe PlayType
determinePlayType state Black  = determineBlackPlayType state
determinePlayType state White  = determineWhitePlayType state


-- |Helper function for determinePlayType;
-- If any square in row 0 contains a Black Pawn, PlayType is PawnPlacement, otherwise it's Normal
-- Returns Nothing if a pawn was upgraded
determineBlackPlayType :: GameState -> Maybe PlayType
determineBlackPlayType state
    | (length [(a,0) | a <- [0..4], (getFromBoard (theBoard state) (a,0)) == BP] > 0) &&
                                     getNumKnights (theBoard state) Black > 1              = Just PawnPlacement
    | (length [(a,0) | a <- [0..4], (getFromBoard (theBoard state) (a,0)) == BP] > 0) &&
                                     getNumKnights (theBoard state) Black < 2              = Nothing
    | otherwise                                                                            = Just Normal


-- |Helper function for determinePlayType;
-- If any square in row 4 contains a White Pawn, PlayType is PawnPlacement, otherwise it's Normal
-- Returns Nothing if a pawn was upgraded
determineWhitePlayType :: GameState -> Maybe PlayType
determineWhitePlayType state
    | (length [(a,4) | a <- [0..4], (getFromBoard (theBoard state) (a,4)) == WP] > 0) &&
                                     getNumKnights (theBoard state) White > 1              = Just PawnPlacement
    | (length [(a,0) | a <- [0..4], (getFromBoard (theBoard state) (a,4)) == WP] > 0) &&
                                     getNumKnights (theBoard state) White < 2              = Nothing
    | otherwise                                                                            = Just Normal


-- | Takes a Board and a Player (Black or White) and returns the number of Knights that player has
getNumKnights :: Board -> Player -> Int
getNumKnights board Black     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == BK]
getNumKnights board White     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == WK]

-- | Takes a Board and a Player (Black or White) and returns the number of Pawns that player has
getNumPawns :: Board -> Player -> Int
getNumPawns board Black     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == BP]
getNumPawns board White     = sum [1 | x <- [0..4], y <- [0..4], getFromBoard board (x,y) == WP]








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







































































