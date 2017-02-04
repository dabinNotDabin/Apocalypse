# Apocalypse

TODO 1 and 2 are also commented out in APOC.hs where they are applicable

TODO 1) check args, if zero, we step into interactive mode (pass over this section of code)
TODO 1)             if two, check that they are legal strategy names. If they are, run playGame with args
TODO 1)                                                               If not, print list of strategy names (see util functions in Apoc.hs)
TODO 1)                                                               see point 6) in functional requirements (spec)
 
TODO 2) check strategy names(a and b), if either are illegal, print a list of strategy names and quit
TODO 2)                                if both legal, run strategies against one another (interactive)


TODO 3 is commented out in ApocStrategyHuman.hs where it needs to be done. Usman said he was thinking of attempting this one.

TODO 3) if playType is Normal, parse MAX 4 integers from input
TODO 3) if playType is PawnPlacement, parse MAX 2 integers from input
TODO 3) note that user input can contain comments so there may be more input following the move coordinates (see spec)
TODO 3) check that all integers, x are such that 0 <= x <= 4 for both types of input.
TODO 3) I assume that if there are more integers than required, the remaining
TODO 3) will be considered a comment but I'll check with Rob
TODO 3) If not enough integers are supplied or integers are out of range, user should be reprompted.
TODO 3) If the user enters say 0 0 3 3, this is a penaltied move but still valid input, don't check for move validity here





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



