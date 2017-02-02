{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
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

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.List
import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import CustomTools


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    putStrLn "  greedy"
    putStrLn "  evasive"
    putStrLn "  human"
    putStrLn "\nChoose a strategy for Black yo"
    a <- getLine
    putStrLn "\nChoose a strategy for White ho"
    b <- getLine
    let blackStrategy = getStrategy a
        whiteStrategy = getStrategy b
    















    putStrLn "\nThe initial board:"
    print initBoard

    putStrLn $ "\nThe initial board with back human (the placeholder for human) strategy having played one move\n"
               ++ "(clearly illegal as we must play in rounds!):"
    move <- blackStrategy (initBoard) Normal Black
    putStrLn (show $ GameState (assessPlay Black Normal move (theBoard initBoard))
                               (blackPen initBoard)
                               (assessPlay White Normal move (theBoard initBoard))
                               (whitePen initBoard)
                               (replace2 (replace2 (theBoard initBoard)
                                                   ((fromJust move) !! 1)
                                                   (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                         ((fromJust move) !! 0)
                                         E))
    
{--

doTurn :: GameState -> Chooser -> Chooser -> GameState
doTurn state blackStrategy whiteStrategy =
    let blackPlayType = determinePlayType state Black 
        whitePlayType = determinePlayType state White
     in 
            | (blackPlayType == Passed) && (whitePlayType == Passed) =  getdoublePassWinner state
            | checkPenalties state Black                             =  doWin White
            | checkPenalties state White                             =  doWin Black
-- account for both accumulate 2 penalty points in the same turn
            | checkForWin    (theBoard state) == Nothing             =  roundSelector state (Just blackPlayType) (Just whitePlayType)
            | checkForWin    (theBoard state) == Just Black          =  doWin Black 
            | checkForWin    (theBoard state) == Just White          =  doWin White







roundSelector :: GameState -> Maybe PlayType -> Maybe PlayType -> GameState
roundSelector state Nothing              Nothing              = runStrategiesUpgrade
roundSelector state Nothing              _                    = runStrategiesUpgrade
roundSelector state _                    Nothing              = runStrategiesUpgrade
--roundSelector state (Just PawnPlacement) (Just PawnPlacement) = runStrategiesPawnPlacement --should get caught from the next two
roundSelector state (Just PawnPlacement) _                    = runStrategiesPawnPlacement
roundSelector state _                    (Just PawnPlacement) = runStrategiesPawnPlacement
roundSelector state (Just Normal)        (Just Normal)        = runStrategiesNormal -- only want to do this if both are normal





checkForWin :: Board -> Maybe Player
checkForWin board
    | ((getNumPawns (theBoard state) Black) == 0)    = Just White
    | ((getNumPawns (theBoard state) White) == 0)    = Just Black
    otherwise                                        = Nothing




checkPenalties :: GameState -> Player -> Bool
checkPenalties state Black = ((blackPen state) >= 2)
checkPenalties state White = ((whitePen state) >= 2)





doWin :: Board -> Player -> Chooser -> Chooser -> IO()
doWin board Black blackStrategy whiteStrategy = do
    putStrLn $ ("Black Wins! (" ++ (chooser2Str blackStrategy) ++ "): " ++ (read (getNumPawns board Black) :: String)
                                ++ (chooser2Str WhiteStrategy) ++ "): " ++ (read (getNumPawns board White) :: String) 
doWin board White blackStrategy whiteStrategy = do
    putStrLn $ ("White Wins! (" ++ (chooser2Str blackStrategy) ++ "): " ++ (read (getNumPawns board Black) :: String)
                                ++ (chooser2Str WhiteStrategy) ++ "): " ++ (read (getNumPawns board White) :: String) 

--}





















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


-- | Moves the piece from the first coordinate to the second coordinate and sets the first coordinate == Cell
swapFill :: Board -> (Int,Int) -> (Int,Int) -> Cell -> Board
swapFill board (w,x) (y,z) cell = replace2 (replace2 board (y,z) (getFromBoard board (w,x))) (w,x) cell


-- | The piece in coordinate 1 and the piece in coordinate 2 are swapped
swap :: Board -> (Int,Int) -> (Int,Int) -> Board
swap board (w,x) (y,z) =
    let pieceB = getFromBoard board (w,x)
        pieceW = getFromBoard board (y,z)
     in  replace2 (replace2 board (y,z) pieceB) (w,x) pieceW





-- | Takes two Cells and returns an Outcome (defined in CustomTools)
--   Used to resolve clashes (two pieces move to the same cell)
--   Expects the Cell of the Black Player as the first arg
getOutcome :: Cell            -- ^ Black Cell
           -> Cell            -- ^ White Cell
           -> Maybe Outcome
getOutcome BP WP = Just Tie
getOutcome BK WK = Just Tie
getOutcome BK WP = Just Win
getOutcome BP WK = Just Loss 


-- | Takes a String and returns a Chooser (strategy)
getStrategy :: String -> Chooser
getStrategy strategy
    | strategy == "greedy"    = greedy
    | strategy == "evasive"   = evasive
    | strategy == "human"     = human
    | otherwise               = human




-- | Takes a Board, a Player (Black or White) and returns the coordinate (if any) that an upgradable pawn exists for that Player
getUpgradablePawnLocation :: Board -> Player -> (Int, Int)
getUpgradablePawnLocation board Black     = ([(a,0) | a <- [0..4], (getFromBoard (board) (a,0)) == BP]) !! 0
getUpgradablePawnLocation board White     = ([(a,4) | a <- [0..4], (getFromBoard (board) (a,4)) == WP]) !! 0





-- Currently, fromJust will throw exceptions when not optimal, need a workaround or else only use this for a normal
-- move phase and create a function selector that decides between run normal, run pawn placement, and run upgrade functions
runStrategiesNormal :: GameState -> Chooser -> Chooser -> IO()
runStrategiesNormal state strat1 strat2 = do
    move1 <- strat1 state Normal Black
    move2 <- strat2 state Normal White
    let moveType      = getMoveType (theBoard state) move1 move2 
        outcome       = getOutcome (getFromBoard (theBoard state) ((fromJust move1) !! 1)) (getFromBoard (theBoard state) ((fromJust move2) !! 1))
        blackPlayed   = assessPlay Black Normal move1 (theBoard state)
        whitePlayed   = assessPlay White Normal move2 (theBoard state)
     in     putStrLn (show $ GameState (blackPlayed)
                                       ((blackPen state) + (getPen blackPlayed))
                                       (whitePlayed)
                                       ((whitePen state) + (getPen whitePlayed)) 
                                       (updateBoard (theBoard state) moveType move1 move2 outcome))



-- | Dummy implementation, not finished
runStrategiesPawnPlacement :: GameState -> Chooser -> Player -> IO()
runStrategiesPawnPlacement state strategy colour = do
    move <- strategy state PawnPlacement colour
    putStrLn "hey"






--can use let to represent getFromBoard board ....  and validityTest outcomes to clean things up
--then use gaurds instead of passing a Bool into getPlay
assessPlay :: Player -> PlayType -> Maybe [(Int, Int)] -> Board -> Played
assessPlay _      Normal        Nothing board  = Passed
assessPlay _      PawnPlacement Nothing board  = NullPlacedPawn
assessPlay colour Normal        move    board  =
    getPlay (validityTest colour (getFromBoard (board) (fromJust move !! 0)) move (getFromBoard (board) (fromJust move !! 1))) colour move board  
assessPlay colour PawnPlacement move    board  =
    getPlay (validityTest' move (getFromBoard (board) (fromJust move !! 0))) colour move board




-- | Checks that certain conditions are met in a normal move only (Not a Pass or Pawn Placement).
--   Returns False if;
--      - a move's source is an empty cell
--      - a move's destination is illegal in regard to the source piece's movement restrictions
--      - a move attempts to move onto a piece of it's own colour
--      - a Player attempts to move another player's piece
--      - a Pawn move to an empty cell is anything but the allowed move of one row forward.
--      - a Pawn move to capture is anything but a diagonal step
--      - a Knight's move in magnitude is anything but 2 rows by 1 column or 2 columns by 1 row
--
--   The Nothing case is not handled as assessPlay handles it. This may not be optimal or best practice.
validityTest :: Player -> Cell                 -- ^ Piece occupying the source cell
                       -> Maybe [(Int, Int)]   -- ^ Move to be made, expects a Normal move
                       -> Cell                 -- ^ Piece occupying the destination cell
                       -> Bool
validityTest _     E   _                       _  = False
validityTest _     BP  _                       BP = False
validityTest _     BP  _                       BK = False
validityTest _     BK  _                       BK = False
validityTest _     BK  _                       BP = False
validityTest _     WP  _                       WP = False
validityTest _     WP  _                       WK = False
validityTest _     WK  _                       WK = False
validityTest _     WK  _                       WP = False
validityTest Black WK  _                       _  = False
validityTest Black WP  _                       _  = False
validityTest White BK  _                       _  = False
validityTest White BP  _                       _  = False
validityTest _     BP (Just [(x0,y0),(x1,y1)]) E  = ((y0 - y1 == 1) && ((x1 - x0) == 0))
validityTest _     BP (Just [(x0,y0),(x1,y1)]) _  = ((y0 - y1 == 1) && ((abs (x1 - x0) == 1)))
validityTest _     WP (Just [(x0,y0),(x1,y1)]) E  = ((y1 - y0 == 1) && ((x1 - x0) == 0))
validityTest _     WP (Just [(x0,y0),(x1,y1)]) _  = ((y1 - y0 == 1) && ((abs (x1 - x0) == 1)))

validityTest _     BK (Just [(x0,y0),(x1,y1)]) _  = (((abs (y0 - y1) == 2) && (abs (x0 - x1) == 1)) ||
                                                    ((abs (y0 - y1) == 1) && (abs (x0 - x1) == 2)))

validityTest _     WK (Just [(x0,y0),(x1,y1)]) _  = (((abs (y0 - y1) == 2) && (abs (x0 - x1) == 1)) ||
                                                    ((abs (y0 - y1) == 1) && (abs (x0 - x1) == 2)))



-- | Checks that certain conditions are met for a Pawn Placement
--   Results in True if the destination (2nd arg) is an empty cell
--   Results in False if the destination (2nd arg) is not empty 
validityTest' :: Maybe [(Int, Int)] -> Cell -> Bool
validityTest' (Just [(x0,y0)]) E = True  
validityTest' (Just [(x0,y0)]) _ = False


-- | Determines the type of play that was made
--   Can be Goofed, Played, PlacedPawn, or BadPlacedPawn
--   All other play types must be determined by examining the state of the board before a move is made or prior to this call.
getPlay :: Bool -> Player -> Maybe [(Int, Int)] -> Board -> Played
getPlay True  _      (Just [(x0,y0),(x1,y1)]) _     = Played ((x0,y0),(x1,y1))
getPlay True  colour (Just [(x0,y0)])         board = PlacedPawn ((getUpgradablePawnLocation board colour),(x0,y0))
getPlay False _      (Just [(x0,y0),(x1,y1)]) _     = Goofed ((x0,y0),(x1,y1))
getPlay False colour (Just [(x0,y0)])         board = BadPlacedPawn ((getUpgradablePawnLocation board colour),(x0,y0))








-- | Returns a MoveType which helps determine the outcome of a set of moves.
--   The second argument should be the move from the Black Strategy, the third argument should be the move from the White Strategy
getMoveType ::  Board -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> MoveType
getMoveType board (Just [(a,b),(c,d)]) (Just [(w,x),(y,z)])
    |  (blackDst == whiteSrc) && (blackSrc == whiteDst)                   = Swap                  -- Players swapped places
    |  (blackDst == whiteSrc)                                             = WhiteDodge            -- Black dest == White src
    |  (whiteDst == blackSrc)                                             = BlackDodge            -- White dest == Black src
    |  (blackDst == whiteDst)                                             = Clash                 -- Destinations are the same
--    |  pieceA == WP || pieceA == WK                                       = BlackCapture
--    |  pieceB == BP || pieceB == BK                                       = WhiteCapture
    where pieceA    = getFromBoard board (c,d)
          pieceB    = getFromBoard board (y,z)
          blackSrc  = (a,b)
          blackDst  = (c,d) 
          whiteSrc  = (w,x)
          whiteDst  = (y,z)



  
-- | Updates the board according to certain conditions.
--   Takes the current baord, a MoveType, a Black move, a White move and a Maybe Outcome(for clashes only -- same destination moves)
--   Default is the last pattern where each source is moved to it's destination (can be a capture) and each source is replaced with E(mpty)
updateBoard :: Board -> MoveType -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Maybe Outcome -> Board
updateBoard board Clash         (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)]) (Just Win)
    = replace2 (swapFill board (w0,x0) (w1,x1) E) (y0,z0) E

updateBoard board Clash         (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)]) (Just Loss)
    = replace2 (swapFill board (y0,z0) (y1,z1) E) (w0,x0) E

updateBoard board Clash         (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)]) (Just Tie)
    = replace2 (replace2 board (w0,x0) E) (y0,z0) E

updateBoard board Swap          (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)]) Nothing
    = swap board (w0,x0) (y0,z0)

updateBoard board WhiteDodge    (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)]) Nothing
    = swapFill board (y0,z0) (y1,z1) (getFromBoard board (w0,x0))

updateBoard board BlackDodge    (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)]) Nothing
    = swapFill board (w0,x0) (w1,x1) (getFromBoard board (y0,z0))

updateBoard board _             (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)]) Nothing
    = swapFill (swapFill board (w0,x0) (w1,x1) E) (y0,z0) (y1,z1) E






-- | Takes a Played type (Passed, Goofed, ...) and returns an integer penalty amount for that play.
getPen :: Played -> Int
getPen (Goofed        ((_,_),(_,_))) = 1
getPen (BadPlacedPawn ((_,_),(_,_))) = 1
getPen  _                            = 0















