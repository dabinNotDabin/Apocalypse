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

module ApocStrategyEvasive where

import ApocTools
import CustomTools
import System.Random
import System.IO.Unsafe




-- | The evasive AI populates a list of moves that the opponent can make, and if an opponent can take the AI's piece, it will move that piece out of the way.
evasive :: Chooser
evasive state Normal colour = 
    let    enemy            = getOppositeColour colour
           playsForEnemy    = getAllPlaysForPlayer (theBoard state) (Just (getPieceLocations (theBoard state) enemy)) enemy
     in do enemyBest <- (getBestofGreedyMoves (filterNothingMoves (getMovesGreedy playsForEnemy)))
           let bestEnemy        = getGreedyDest (theBoard state) colour enemyBest
               movePieceMoves   = populateMoveList (theBoard state) bestEnemy colour
            in (getMoveEvasive movePieceMoves (theBoard state) colour)




evasive state PawnPlacement _ = return (Just [getEmptyCells (theBoard state)])



 
getOppositeColour :: Player -> Player
getOppositeColour colour | colour == Black = White
                         | colour == White = Black

--| generates a move based on the list of opponent's moves. If the opponent can not immediately take our piece, make a move at random
getMoveEvasive :: MovesForPiece -> Board -> Player -> IO (Maybe [(Int,Int)])
getMoveEvasive (x,[]) board colour = 
    let playsForPlayer = getAllPlaysForPlayer board (Just (getPieceLocations board colour)) colour
     in (getBestofGreedyMoves (filterNothingMoves (getMovesGreedy playsForPlayer)))
getMoveEvasive (x,xs) board colour
    | randomNum > 5           = 
       let playsForPlayer = getAllPlaysForPlayer board (Just (getPieceLocations board colour)) colour
        in (getBestofGreedyMoves (filterNothingMoves (getMovesGreedy playsForPlayer)))
    | otherwise               = 
       let playsForPlayer = getAllPlaysForPlayer board (Just [x]) colour
        in (getBestofGreedyMoves (filterNothingMoves (getMovesGreedy playsForPlayer)))




--| return destination coordinates for the opponent's best move.
getGreedyDest :: Board -> Player -> Maybe [(Int,Int)] -> (Int, Int) -- returns destination coords for enemy's best move
getGreedyDest board colour Nothing = 
    let pieceLocations = (getPieceLocations board colour)
     in pieceLocations !! (unsafePerformIO (randomRIO (0, ((length pieceLocations) - 1))))
getGreedyDest board colour (Just [x,y])
    | (getFromBoard board y == E)  =
        let pieceLocations = (getPieceLocations board colour)
         in pieceLocations !! (unsafePerformIO (randomRIO (0, ((length pieceLocations) - 1))))

getGreedyDest board colour (Just [x,y]) = y

