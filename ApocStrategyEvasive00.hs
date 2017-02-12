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

module ApocStrategyEvasive00 where

import ApocTools
import CustomTools
import System.Random
import System.IO.Unsafe




evasive :: Chooser
evasive state Normal colour = do
    let enemy            = getOppositeColour colour
        playsForEnemy    = getAllPlaysForPlayer (theBoard state) (Just (getPieceLocations (theBoard state) enemy)) enemy
        bestEnemy        = getGreedyDest (theBoard state) colour (getBestofGreedyMoves (filterNothingMoves (getMoveGreedy playsForEnemy)))              --grab enemies best move for comparison
        movePieceMoves   = populateMoveList (theBoard state) bestEnemy colour
        playsForPlayer   = getAllPlaysForPlayer (theBoard state) (Just (getPieceLocations (theBoard state) colour)) colour
     in return (getMoveEvasive movePieceMoves)



evasive b PawnPlacement c = return (Just [getEmptyCells (theBoard b)])



 
getOppositeColour :: Player -> Player
getOppositeColour colour | colour == Black = White
                         | colour == White = Black

getMoveEvasive :: MovesForPiece -> Maybe [(Int,Int)]
getMoveEvasive (x,[]) = Nothing
getMoveEvasive (x,zs) = Just [x , getMoveEvasive' zs ]

getMoveEvasive' :: [((Int,Int), PlayOption)] -> (Int,Int)
getMoveEvasive' [(x,y)] = x
getMoveEvasive' (z:zs) 
    | (snd z) == EmptyCell = (fst z)
    | otherwise      = getMoveEvasive' zs


--Might need to catch a Nothing case here.. for example, the Player has 1 pawn and it's blocked directly in front by an opposite pawn and no captures are available
getGreedyDest :: Board -> Player -> Maybe [(Int,Int)] -> (Int, Int) -- returns destination coords for enemy's best move
--getGreedyDest board colour Nothing = (0,0)
getGreedyDest board colour Nothing = 
    let pieceLocations = (getPieceLocations board colour)
     in pieceLocations !! (unsafePerformIO (randomRIO (0, ((length pieceLocations) - 1))))
getGreedyDest board colour (Just [x,y])
    | (getFromBoard board y == E)  =
        let pieceLocations = (getPieceLocations board colour)
         in pieceLocations !! (unsafePerformIO (randomRIO (0, ((length pieceLocations) - 1))))

getGreedyDest board colour (Just [x,y]) = y



