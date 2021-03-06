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

module ApocStrategyGreedy where

import ApocTools
import CustomTools
import System.Random
import System.IO.Unsafe






greedy    :: Chooser
greedy state Normal colour = 
    let playsForPlayer = getAllPlaysForPlayer (theBoard state) (Just (getPieceLocations (theBoard state) colour)) colour
     in (getBestofGreedyMoves (filterNothingMoves (getMovesGreedy playsForPlayer)))


greedy state PawnPlacement _ = return (Just [getEmptyCells (theBoard state)])



