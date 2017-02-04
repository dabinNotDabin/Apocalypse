# Apocalypse

TODO : TODO clauses can be found in the .hs files where they are relevant

Notes : If a player passes on a pawn placement, it's a penalty.
      : If a player misses a capture, it's not a penalty (unfortunately our idea won't work)
      : If both players reach  2 penalty points in the same round, it's a tie
      : If both players pass the same round it's a tie and the game ends
      : Rob said he isn't sure if, in the case that two pawns are to be upgraded on the same turn, the
        output should be done in one step or two but we can write a script where that happens and he'll try it.
      : With user input, a PlayType of Passed must be recieved as an empty line without a comment,
        if the input has no leading integers ie "hey" the user should be reprompted. 


For Main

Whichever function we get input in ... 
ie;
    move1 <- blackStrategy state Normal Black
    move2 <- whiteStrategy state Normal White
    
... must have an IO (...) return type, can be IO GameState, IO(), etc.
This is because the operator <- must be within a do block

We can create a playGame function that is like our highest order function but it has to return some IO type.
If it takes two chooser types, it can get the moves from the strategies as shown above
where the game always starts with;
  state    ==   initBoard and 
  PlayType ==   Normal (for both players)
 

   -- we can have it print all results as it goes, calling itself recursively (if no win state detected)
   -- on finding a win state, it can print out the results of the match satisfying the IO() return type
   

    
playGame :: Bool -> GameState -> Chooser -> Chooser -> IO()
playGame False state blackStrat whiteStrat
    | pawnUpgradeRequired       = do
        ...   runUpgrade  ....
        ... print results ....
        playGame winCondition? newState blackStrat whiteStrat   -- if a Player upgrades their last pawn, the other Player wins
    | not pawnUpgradeRequired   = do
        let blackPlayType = determinePlayType .....
            whitePlayType = determinePlayType .....
        move1 <- blackStrat state blackPlayType Black
        move2 <- whiteStrat state whitePlayType White
        ... execute round ....
        ... print results ....
        playGame winCondition? newState blackStrat whiteStrat
    
playGame True state blackStrat whiteStrat = do
    do output for winning conditions satisfying IO() return type



just my idea that'll work well with what I already have written.



