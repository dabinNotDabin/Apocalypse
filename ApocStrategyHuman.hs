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

module ApocStrategyHuman (
   human,
   greedy,
   evasive
   ) where

import ApocTools

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human    :: Chooser
human state playType colour = do
    promptUser playType colour
    input <- getLine
--check that user input is either 7 characters, 3 characters or 1 character
    return $ inputToCoordinates ([read [x] :: Int | x <- input, elem x ['0'..'4']])



greedy    :: Chooser
greedy b Normal        c = return (Just [(0,4),(2,3)])
greedy b PawnPlacement c = return (Just [(2,2)])



evasive    :: Chooser
evasive b Normal        c = return (Just [(4,4),(3,2)])
evasive b PawnPlacement c = return (Just [(2,2)])














-- | Gets a move from the user.
-- If the move is a normal move, a list of the form [(FromX, FromY),(ToX, ToY)] is returned.
-- If the move is a pawn placement. a list of the form [(ToX, ToY)] is returned
-- If the move is a pass, Nothing is returned.
--getNormalMove :: Maybe [(x,y),(a,b)] -> [(x,y),(a,b)] 
--getNormalMove Just [(x,y),(a,b)]




-- | Arrange a list of integers into a set of coordinates. If the list is of length 4, a pair of coordinates will be returned
-- If the list is of length 2, a singleton list containing a coordinate will be returned.
-- If the list is of length 0, Nothing is returned.
-- Example, [1,2,3,4] -> [(1,2),(3,4)] ....... [1,2] -> [(1,2)] ......... [] -> Nothing
-- This function expects input handling to be 
inputToCoordinates :: [Int] -> Maybe [(Int, Int)]
inputToCoordinates [x0, y0, x1, y1]  = Just [(x0,y0),(x1,y1)]
inputToCoordinates [x0, y0]          = Just [(x0,y0)]
inputToCoordinates []                = Nothing
inputToCoordinates _                 = Nothing


promptUser :: PlayType -> Player -> IO()
promptUser PawnPlacement colour = putStrLn $ "Enter the move coordinates for player " ++ show colour
                                          ++ " in the form 'srcX srcY destX destY'"
                                          ++ "[0 >= n >= 4, or just enter return for a 'pass'] "
                                          ++ head (show colour) : "2:"
promptUser Normal        colour = putStrLn $ "Enter the move coordinates for player " ++ show colour
                                          ++ " in the form 'srcX srcY destX destY'"
                                          ++ "[0 >= n >= 4, or just enter return for a 'pass'] "
                                          ++ head (show colour) : "4:" 














--             = do putStrLn "Invalid input: " ++ show inp ++ " --> You must enter input in the form;"
--	                                      ++ "\nw x y z  where 0 <= w,x,y,z <= 4    or \nx y where 0 <= x,y <= 4."







































