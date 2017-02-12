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
      replace, replace2, moveAndFill, swap,
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
import System.Exit
import ApocTools
import ApocStrategyHuman
import ApocStrategyGreedy
import ApocStrategyEvasive
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
    (a, b, isNotInteractive) <- interpretArgs args
    checkmode a b isNotInteractive





playGame :: Int -> Bool -> GameState -> Chooser -> Chooser -> IO()
playGame x False state blackStrat whiteStrat
    | (pawnUpgradeRequired (determinePlayType state Black) == True)  ||  (pawnUpgradeRequired (determinePlayType state White) == True)    = do
        let newState = upgradePawns state (pawnUpgradeRequired (determinePlayType state White)) (pawnUpgradeRequired (determinePlayType state Black))
         in do a <- checkWin newState
               print state
               checkWin newState
               playGame (x+1) a newState blackStrat whiteStrat
    | otherwise  = do
        let blackPlayType = determinePlayType state Black
            whitePlayType = determinePlayType state White
         in do print state
               newState <- roundExecute x state blackStrat blackPlayType whiteStrat whitePlayType
               a <- checkWin newState
               playGame (x+1) a newState blackStrat whiteStrat

playGame x True state blackStrat whiteStrat = do print state




    
checkmode :: Chooser -> Chooser -> Bool -> IO()
checkmode blackStrat whiteStrat isNotInteractive
    | isNotInteractive == False    = interactiveMode
    | otherwise                    = playGame 0 False initBoard blackStrat whiteStrat

 
 

interactiveMode :: IO()
interactiveMode = do
    putStrLn "  greedy"
    putStrLn "  evasive"
    putStrLn "  human"
    putStrLn "\nChoose a strategy for Black yo"
    a <- getLine
    putStrLn "\nChoose a strategy for White tho"
    b <- getLine
    let blackStrat = getStrategy a
        whiteStrat = getStrategy b
    playGame 0 False initBoard blackStrat whiteStrat








-- | Moves the piece from the first coordinate to the second coordinate and sets the first coordinate == Cell
moveAndFill :: Board -> (Int,Int) -> (Int,Int) -> Cell -> Board
moveAndFill board (w,x) (y,z) cell = replace2 (replace2 board (y,z) (getFromBoard board (w,x))) (w,x) cell


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




 
 
checkWin :: GameState -> IO(Bool)
checkWin gamestate = do
    a <- checkWinByPawn Black gamestate
    b <- checkWinByPawn White gamestate
    c <- checkLossThroughPenalty Black gamestate
    d <- checkLossThroughPenalty White gamestate
    e <- checkForDoublePass gamestate
    return (a || b || c || d || e)

checkWinByPawn :: Player -> GameState -> IO(Bool)
checkWinByPawn player gamestate =
    if player == Black
        then let aBoard = theBoard gamestate
             in (checkWhitePawns aBoard)
    else
        let aBoard = theBoard gamestate
        in (checkBlackPawns aBoard)

checkBlackPawns :: [[Cell]] -> IO(Bool)
checkBlackPawns [] = do putStrLn $ "White Wins by taking all Pawns!"; return True
checkBlackPawns (x:xs)   | elem BP x == True = do return False
                         | elem BP x == False = checkBlackPawns xs

checkWhitePawns :: [[Cell]] -> IO(Bool)
checkWhitePawns [] = do putStrLn $ "Black Wins by taking all Pawns!"; return True
checkWhitePawns (x:xs)   | elem WP x == True = do return False
                         | elem WP x == False = checkWhitePawns xs

checkLossThroughPenalty :: Player -> GameState -> IO(Bool)
checkLossThroughPenalty player gamestate 
    | ((player == Black) && (checkPenalties gamestate player )) = do putStrLn $ "White Wins by penalty!"; return True
    | ((player == White) && (checkPenalties gamestate player )) = do putStrLn $ "Black Wins by penalty!"; return True
    | True = do return False

checkForDoublePass :: GameState -> IO(Bool)
checkForDoublePass state
    | (blackPlay state == Passed) && (whitePlay state == Passed) =  do putStrLn $ "Tie -- Double Pass"; return True
    | otherwise                                                  = return False


 
 
 
 
 
 
 

-- ========================================================================================
-- ===================Supervisory Functions -- Used for Control of Game====================
-- ========================================================================================




-- | Takes two moves and a 'GameState', assuming the first to be Black's move and the second to be White's move and executes one 'Normal' turn.
runStrategiesNormal :: Int -> GameState -> Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> GameState
runStrategiesNormal x state blackMove whiteMove
    | mod x 21 == 20  = 
        let blackPlayed = assessPlay Black Normal blackMove (theBoard state)
            whitePlayed = assessPlay White Normal Nothing (theBoard state)
         in      GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)
    | mod x 22 == 21  = 
        let blackPlayed = assessPlay Black Normal Nothing (theBoard state)
            whitePlayed = assessPlay White Normal whiteMove (theBoard state)
         in      GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)
    | mod x 42 == 41  = 
        let blackPlayed = assessPlay Black Normal Nothing (theBoard state)
            whitePlayed = assessPlay White Normal Nothing (theBoard state)
         in      GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)
    | otherwise       =
        let blackPlayed = assessPlay Black Normal blackMove (theBoard state)
            whitePlayed = assessPlay White Normal whiteMove (theBoard state)
         in      GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)









-- | Takes two moves and a 'GameState' assuming the first to be Black's move and the second to be White's move and executes one 'PawnPlacement' turn. 
--   Both players can place pawns on the same round. If just one 'Player' is placing on a turn, pass Nothing in for the other 'Player'
--   Can be called with Nothing for both players' moves and will execute an upgrade pawn to knight.
runStrategiesPawnPlacement :: GameState -> Maybe[(Int,Int)] -> Maybe[(Int,Int)] -> GameState
runStrategiesPawnPlacement state Nothing   Nothing   =
                 GameState (getPlay True Black Nothing (theBoard state))
                           ((blackPen state))
                           (getPlay True Black Nothing (theBoard state))
                           ((whitePen state)) 
                           (updateBoard (theBoard state) None None)

runStrategiesPawnPlacement state Nothing   whiteMove = 
    let blackPlayed = None 
        whitePlayed = assessPlay White PawnPlacement whiteMove (theBoard state)
     in          GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)

 
runStrategiesPawnPlacement state blackMove Nothing   = 
    let blackPlayed = assessPlay Black PawnPlacement blackMove (theBoard state)
        whitePlayed = None
     in          GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)

runStrategiesPawnPlacement state blackMove whiteMove = 
    let blackPlayed   = assessPlay Black PawnPlacement blackMove (theBoard state)
        whitePlayed   = assessPlay White PawnPlacement whiteMove (theBoard state)
     in           GameState (blackPlayed)
                           ((blackPen state) + (getPen blackPlayed))
                           (whitePlayed)
                           ((whitePen state) + (getPen whitePlayed)) 
                           (updateBoard (theBoard state) blackPlayed whitePlayed)






roundExecute :: Int -> GameState -> Chooser -> Maybe PlayType -> Chooser -> Maybe PlayType -> IO GameState
    
roundExecute x state blackStrat (Just Normal)        whiteStrat (Just Normal)          = do
    move1 <- blackStrat state Normal Black
    move2 <- whiteStrat state Normal White
    return (runStrategiesNormal x state move1 move2)

roundExecute x state blackStrat (Just PawnPlacement) whiteStrat (Just PawnPlacement)   = do 
    move1 <- blackStrat state PawnPlacement Black
    move2 <- whiteStrat state PawnPlacement White
    return (runStrategiesPawnPlacement state move1 move2)

roundExecute x state blackStrat _                    whiteStrat (Just PawnPlacement)   = do 
    move2 <- whiteStrat state PawnPlacement White
    return (runStrategiesPawnPlacement state Nothing move2) 
    
roundExecute x state blackStrat (Just PawnPlacement) whiteStrat  _                     = do 
    move1 <- blackStrat state PawnPlacement Black
    return (runStrategiesPawnPlacement state move1 Nothing)









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
--   Can be Goofed, Played, PlacedPawn, UpgradedPawn2Knight, or BadPlacedPawn
--   Passed and NullPlacedPawn must be determined prior to or proceeding this call.
getPlay :: Bool -> Player -> Maybe [(Int, Int)] -> Board -> Played
getPlay True  colour  Nothing                 board = getPlay' board colour 
getPlay True  _      (Just [(x0,y0),(x1,y1)]) _     = Played ((x0,y0),(x1,y1))
getPlay True  colour (Just [(x0,y0)])         board = PlacedPawn ( ((getUpgradablePawnLocation board colour) !! 0),(x0,y0))
getPlay False _      (Just [(x0,y0),(x1,y1)]) _     = Goofed ((x0,y0),(x1,y1))
getPlay False colour (Just [(x0,y0)])         board
    | len == 0     = BadPlacedPawn ((1,2),(x0,y0))
    | otherwise    = BadPlacedPawn ( ((getUpgradablePawnLocation board colour) !! 0),(x0,y0))
    where len = length (getUpgradablePawnLocation board colour)

-- | Helper function for getPlay, used when determining whether a pawn upgrade occurred.
getPlay' :: Board -> Player -> Played
getPlay' board colour
    | length upgradablePawnLocation /= 0     = UpgradedPawn2Knight $ upgradablePawnLocation !! 0
    | otherwise                              = None
    where upgradablePawnLocation = getUpgradablePawnLocation board colour





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
--updateBoard board None                        None                                    = upgrade2Knight board

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

updateBoard' board WhiteDodge    (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])    = replace2 (moveAndFill board (y0,z0) (y1,z1) (getFromBoard board (w0,x0))) (w0,x0) E

updateBoard' board BlackDodge    (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])    = replace2 (moveAndFill board (w0,x0) (w1,x1) (getFromBoard board (y0,z0))) (y0,z0) E

updateBoard' board NoEvent       (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])    = moveAndFill (moveAndFill board (w0,x0) (w1,x1) E) (y0,z0) (y1,z1) E

updateBoard' board NoEvent        Nothing                 (Just [(y0,z0),(y1,z1)])    = moveAndFill board (y0,z0) (y1,z1) E

updateBoard' board NoEvent       (Just [(w0,x0),(w1,x1)]) Nothing                     = moveAndFill board (w0,x0) (w1,x1) E 






doUpdateClash :: Board -> Maybe [(Int, Int)] -> Maybe [(Int, Int)] -> Board
doUpdateClash board (Just [(w0,x0),(w1,x1)]) (Just [(y0,z0),(y1,z1)])
    | outcome  ==  Win     = replace2 (moveAndFill board (w0,x0) (w1,x1) E) (y0,z0) E
    | outcome  ==  Loss    = replace2 (moveAndFill board (y0,z0) (y1,z1) E) (w0,x0) E
    | outcome  ==  Tie     = replace2 (replace2 board (w0,x0) E) (y0,z0) E
    where outcome = (getOutcome (getFromBoard board (w0,x0)) (getFromBoard board (y0,z0)))









-- ========================================================================================
-- ====================================Control of Flow=====================================
-- ========================================================================================


{- | Return True only if we find "-v" among the command-line arguments.  If the command
line is empty, then return False.  If the commmand-line is not empty and doesn't
contain -v, then print a synopsis and exit.
-}
interpretArgs :: [String] -> IO (Chooser, Chooser, Bool)
interpretArgs args | (args == []) = do return (human, human, False)
                   | ((head args) == "human") && (elem "human" (tail args))  && (length args == 2)     = do return (human, human, True)
                   | ((head args) == "human") && (elem "greedy" (tail args))  && (length args == 2)    = do return (human, greedy, True)
                   | ((head args) == "human") && (elem "evasive" (tail args))  && (length args == 2)   = do return (human, evasive, True)
                   | ((head args) == "greedy") && (elem "human" (tail args))  && (length args == 2)    = do return (greedy, human, True)
                   | ((head args) == "greedy") && (elem "greedy" (tail args))  && (length args == 2)   = do return (greedy, greedy, True)
                   | ((head args) == "greedy") && (elem "evasive" (tail args))  && (length args == 2)  = do return (greedy, evasive, True)
                   | ((head args) == "evasive") && (elem "human" (tail args))  && (length args == 2)   = do return (evasive, human, True)
                   | ((head args) == "evasive") && (elem "greedy" (tail args))  && (length args == 2)  = do return (evasive, greedy, True)
                   | ((head args) == "evasive") && (elem "evasive" (tail args))  && (length args == 2) = do return (evasive, evasive, True)
                   |  otherwise                                                                        = do printSynopsis; return (human, human, False)



{- | Print a synopsis of the program.
-}
printSynopsis :: IO ()
printSynopsis = do
name <- getProgName
putStrLn "\nUsage:"
putStrLn $ "  " ++ name ++ "\n"
putStrLn "1st arg = (human, greedy, evasive), 2nd arg = (human, greedy, evasive)"
putStrLn "The first argument chooses a strategy for black, and the second argument chooses a strategy for white."
putStrLn "If you decide to choose a strategy for black, you must also choose a strategy for white."
putStrLn "You can also decide to play using the interactive mode by running the program with no arguments. "
putStrLn "Anything else on the command line causes this message to be printed and"
putStrLn "the program will exit.\n"






