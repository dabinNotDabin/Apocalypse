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
      replace, replace2, moveFill, swap,
      -- * Other Util Functions
      getPen, getOutcome, getStrategy, printStrategies, getUpgradablePawnLocation, checkForZeroPawns, checkPenalties,
      -- * Supervisory Functions -- Control Flow of the Game
      runStrategiesNormal,
      -- * Move Validation
      assessPlay, getPlay, validityTest, validityTest',
      -- * Alteration of Game State
      getMoveType, updateBoard, updateBoard', doUpdateClash
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
-------------------------------------------------------------------------------------TODO 1) check args, if zero, we step into interactive mode (pass over this section of code)
-------------------------------------------------------------------------------------TODO 1)             if two, check that they are legal strategy names. If they are, run playGame with args
-------------------------------------------------------------------------------------TODO 1)                                                               If not, do print list of strategy names
-------------------------------------------------------------------------------------TODO 1)                                                               see point 6) in functional requirements (spec)
 

    let strategies = [(human, "human"),(greedy, "greedy"),(evasive, "evasive")] 

    putStrLn "  greedy"
    putStrLn "  evasive"
    putStrLn "  human"
    putStrLn "\nChoose a strategy for Black yo"
    a <- getLine
    putStrLn "\nChoose a strategy for White tho"
    b <- getLine
    
            
-------------------------------------------------------------------------------------TODO 2) check strategy names(a and b), if either are illegal, print a list of strategy names and quit
-------------------------------------------------------------------------------------TODO 2)                                if both legal, run strategies against one another (interactive)



    
    let blackStrategy = getStrategy a
        whiteStrategy = getStrategy b

    print testBoard2

    
    blackMove <- blackStrategy testBoard2 Normal Black
    whiteMove <- whiteStrategy testBoard2 Normal White

  
    print (runStrategiesNormal testBoard2 blackMove whiteMove)
    










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







roundSelector :: GameState -> Maybe PlayType -> Maybe PlayType -> IO()
roundSelector state (Just PawnPlacement) (Just PawnPlacement)   = runStrategiesPawnPlacement
roundSelector state _                    (Just PawnPlacement)   = runStrategiesPawnPlacement
roundSelector state (Just PawnPlacement) _                      = runStrategiesPawnPlacement
roundSelector state (Just Normal)        (Just Normal)          = runStrategiesNormal -- only want to do this if both are normal





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
moveFill :: Board -> (Int,Int) -> (Int,Int) -> Cell -> Board
moveFill board (w,x) (y,z) cell = replace2 (replace2 board (y,z) (getFromBoard board (w,x))) (w,x) cell


-- | The piece in coordinate 1 and the piece in coordinate 2 are swapped
swap :: Board -> (Int,Int) -> (Int,Int) -> Board
swap board (w,x) (y,z) =
    let pieceB = getFromBoard board (w,x)
        pieceW = getFromBoard board (y,z)
     in  replace2 (replace2 board (y,z) pieceB) (w,x) pieceW













-- ========================================================================================
-- =============================Short Util Functions=======================================
-- ========================================================================================


-- | Takes a 'Played' type (Passed, Goofed, ...) and returns an integer penalty amount for that play.
getPen :: Played -> Int
getPen (Goofed        ((_,_),(_,_))) = 1
getPen (BadPlacedPawn ((_,_),(_,_))) = 1
getPen  NullPlacedPawn               = 1
getPen  _                            = 0




-- | Takes two 'Cell' s and returns an 'Outcome'
--   Used to resolve 'Clash' es
--   Expects the 'Cell' of the 'Black' 'Player' as the first arg, 'White' 'Player' as the second
getOutcome :: Cell            -- ^ Black Cell
           -> Cell            -- ^ White Cell
           -> Outcome
getOutcome BP WP = Tie
getOutcome BK WK = Tie
getOutcome BK WP = Win
getOutcome BP WK = Loss




-- | Takes a String and returns a 'Chooser' (strategy)
getStrategy :: String -> Chooser
getStrategy strategy
    | strategy == "greedy"    = greedy
    | strategy == "evasive"   = evasive
    | strategy == "human"     = human
    | otherwise               = human





-- | Takes a list of tuples, 'Chooser' paired with it's 'String' name and prints the list with two spaces before each strategy separated with a new line
printStrategies :: [(Chooser, String)] -> IO()
printStrategies []     = putStr ""
printStrategies (x:xs) = do
    putStrLn ("  " ++ (snd x))
    printStrategies xs




-- | Returns the coordinate (if any) that an upgradable pawn exists for the specified 'Player'
getUpgradablePawnLocation :: Board -> Player -> (Int, Int)
getUpgradablePawnLocation board Black     = ([(a,0) | a <- [0..4], (getFromBoard (board) (a,0)) == BP]) !! 0
getUpgradablePawnLocation board White     = ([(a,4) | a <- [0..4], (getFromBoard (board) (a,4)) == WP]) !! 0





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




 
 
 
 
 
 
 
 
 
 

-- ========================================================================================
-- ===================Supervisory Functions -- Used for Control of Game====================
-- ========================================================================================




-- | Takes two moves, assuming the first to be Black's move and the second to be White's move and executes one turn.
runStrategiesNormal :: GameState -> Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> GameState
runStrategiesNormal state blackMove whiteMove = 
    let blackPlayed   = assessPlay Black Normal blackMove (theBoard state)
        whitePlayed   = assessPlay White Normal whiteMove (theBoard state)
     in          GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)







{--
-- | Not Done
runStrategiesPawnPlacement :: GameState -> Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> GameState
runStrategiesPawnPlacement state blackMove whiteMove = 
    let blackPlayed   = assessPlay Black Normal blackMove (theBoard state)
        whitePlayed   = assessPlay White Normal whiteMove (theBoard state)
     in           GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)




--} 












-- ========================================================================================
-- ==========Functions Used to Validate a Move and Return it's 'Played' Type===============
-- ========================================================================================




-- | Used to determine a 'Played' Type based on the Player, PlayType, a move and the Board
assessPlay :: Player -> PlayType -> Maybe [(Int, Int)] -> Board -> Played
assessPlay _      Normal        Nothing board  = Passed
assessPlay _      PawnPlacement Nothing board  = NullPlacedPawn
assessPlay colour Normal        move    board  =
    let cellA = (getFromBoard (board) (fromJust move !! 0))
        cellB = (getFromBoard (board) (fromJust move !! 1))
     in 
        getPlay (validityTest colour cellA move cellB) colour move board  

assessPlay colour PawnPlacement move    board  =
    let cell = (getFromBoard (board) (fromJust move !! 0))
     in 
        getPlay (validityTest' move cell) colour move board






-- | Determines the type of play that was made
--   Can be Goofed, Played, PlacedPawn, or BadPlacedPawn
--   All other play types must be determined by examining the state of the board before a move is made or prior to this call.
getPlay :: Bool -> Player -> Maybe [(Int, Int)] -> Board -> Played
getPlay True  _      (Just [(x0,y0),(x1,y1)]) _     = Played ((x0,y0),(x1,y1))
getPlay True  colour (Just [(x0,y0)])         board = PlacedPawn ((getUpgradablePawnLocation board colour),(x0,y0))
getPlay False _      (Just [(x0,y0),(x1,y1)]) _     = Goofed ((x0,y0),(x1,y1))
getPlay False colour (Just [(x0,y0)])         board = BadPlacedPawn ((getUpgradablePawnLocation board colour),(x0,y0))











-- | Checks that certain conditions are met in a NORMAL MOVE ONLY (Not a Pass or Pawn Placement).
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









-- | Checks that certain conditions are met for a PAWN PLACEMENT ONLY
--   Results in True if the destination (2nd arg) is an empty cell
--   Results in False if the destination (2nd arg) is not empty 
validityTest' :: Maybe [(Int, Int)] -> Cell -> Bool
validityTest' (Just [(x0,y0)]) E = True  
validityTest' (Just [(x0,y0)]) _ = False

















-- ========================================================================================
-- =================================Update Board Functions=================================
-- ========================================================================================





-- | Returns a MoveType which helps determine how to update the board after a set of moves.
--   The MoveType is only needed to describe a set of Normal Moves, all other combinations result in NoEvent
--   The second argument should be the move from the Black Strategy, the third argument should be the move from the White Strategy
--   Only valid if moves passed in are valid.
getMoveType ::  Board -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> MoveType
getMoveType board (Just [(a,b),(c,d)]) (Just [(w,x),(y,z)])
    |  (blackDst == whiteSrc) && (blackSrc == whiteDst)                   = Swap                  -- Players swapped places
    |  (blackDst == whiteSrc)                                             = WhiteDodge            -- Black dest == White src
    |  (whiteDst == blackSrc)                                             = BlackDodge            -- White dest == Black src
    |  (blackDst == whiteDst)                                             = Clash                 -- Destinations are the same
    |  otherwise                                                          = NoEvent               -- None of the above
    where pieceA    = getFromBoard board (c,d)
          pieceB    = getFromBoard board (y,z)
          blackSrc  = (a,b)
          blackDst  = (c,d) 
          whiteSrc  = (w,x)
          whiteDst  = (y,z)

getMoveType _     _     _                                                 = NoEvent               -- If not two normal moves, noEvent is automatic


  
  
  
  
  
  
  
-- | Checks which moves to apply according to the Played arguments.
--   Takes the current board, a MoveType, a Black Played, a White Played and returns an updated board
--   If both Players Played, moveType is needed to determine outcome for possible clashes, dodges etc.
--   If both Players PlacedPawn, moveType is needed to determine outcome in the case of a clash.
--   All other combinations are NoEvent moves and there are four possible combinations of NoEvent moves
--      - that will affect the board and the last pattern is a catch all that does not apply changes.
updateBoard :: Board -> Played -> Played -> Board

updateBoard board (Played ((w0,x0),(w1,x1))) (Played ((y0,z0),(y1,z1)))               = 
    let moveType = getMoveType board (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])
     in 
        updateBoard' board moveType (Just [(w0,x0),(w1,x1)])  (Just [(y0,z0),(y1,z1)])


updateBoard board (PlacedPawn ((w0,x0),(w1,x1))) (PlacedPawn ((y0,z0),(y1,z1)))       =   
    let moveType = getMoveType board (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])
     in 
        updateBoard' board moveType (Just [(w0,x0),(w1,x1)])  (Just [(y0,z0),(y1,z1)])

   
updateBoard board (Played ((w0,x0),(w1,x1)))        _                                 =   updateBoard' board NoEvent  (Just [(w0,x0),(w1,x1)])  Nothing
          
updateBoard board  _                               (Played ((y0,z0),(y1,z1)))         =   updateBoard' board NoEvent   Nothing                 (Just [(y0,z0),(y1,z1)])

updateBoard board (PlacedPawn ((w0,x0),(w1,x1)))  _                                   =   updateBoard' board NoEvent  (Just [(w0,x0),(w1,x1)])  Nothing
          
updateBoard board  _                               (PlacedPawn ((y0,z0),(y1,z1)))     =   updateBoard' board NoEvent   Nothing                 (Just [(y0,z0),(y1,z1)])

updateBoard board  _                               _                                  =   board




 
  
-- | Helper function for updateBoard -- actually executes the update whereas it's caller checks which moves to apply
updateBoard' :: Board -> MoveType -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Board

updateBoard' board Clash         blackMove                whiteMove                   = doUpdateClash board blackMove whiteMove

updateBoard' board Swap          (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])    = swap board (w0,x0) (y0,z0)

updateBoard' board WhiteDodge    (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])    = moveFill board (y0,z0) (y1,z1) (getFromBoard board (w0,x0))

updateBoard' board BlackDodge    (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])    = moveFill board (w0,x0) (w1,x1) (getFromBoard board (y0,z0))

updateBoard' board NoEvent       (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])    = moveFill (moveFill board (w0,x0) (w1,x1) E) (y0,z0) (y1,z1) E

updateBoard' board NoEvent        Nothing                 (Just [(y0,z0),(y1,z1)])    = moveFill board (y0,z0) (y1,z1) E

updateBoard' board NoEvent       (Just [(w0,x0),(w1,x1)]) Nothing                     = moveFill board (w0,x0) (w1,x1) E 






doUpdateClash :: Board -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Board
doUpdateClash board (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])
    | (getOutcome (getFromBoard board (w0,x0)) (getFromBoard board (y0,z0)))      ==     Win     = replace2 (moveFill board (w0,x0) (w1,x1) E) (y0,z0) E
    | (getOutcome (getFromBoard board (w0,x0)) (getFromBoard board (y0,z0)))      ==     Loss    = replace2 (moveFill board (y0,z0) (y1,z1) E) (w0,x0) E
    | (getOutcome (getFromBoard board (w0,x0)) (getFromBoard board (y0,z0)))      ==     Tie     = replace2 (replace2 board (w0,x0) E) (y0,z0) E







