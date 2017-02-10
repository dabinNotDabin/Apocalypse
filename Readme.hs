# Apocalypse

Notes at the end may be important for some of these TODO clauses

TODO -4) Function checkForWin :: GameState -> Bool that checks fora  winCondition (penalties == 2 or pawns == 0)

TODO -3) Function called upgrade2Knight :: Board -> Board which finds one pawn to upgrade and exchanges it with a knight
         using the util function replace2. ** It may have to upgrade both (if more than one) in one round rather than one
         at a time. I asked Rob and he said he's not sure but if we work on TODO -2.5)
         
TODO-2.5) Develop a script that tests for the scenario in which both players move a pawn for an upgrade
          (both have less than two knights) that we can send to him, he'll try it on his and let me know.

TODO -2) Function to check (using the GameState as a parameter) whether a pawn upgrade or pawn placement is required.

TODO -1) Write test scripts to ensure our program meets the requirements. 
         (See Notes at the very end of this readme for some ides to start with

TODO 0)**** Get Apoc.hs functional enough to run in ApocCheckerMini to guage where we're at for functionality and correctness.

TODO 1 and 2 can be found as comments in APOC.hs where they are applicable

TODO 1) check args, if zero, we step into interactive mode (pass over this section of code)
TODO 1)             if two, check that they are legal strategy names. If they are, run playGame with args
TODO 1)                                                               If not, print list of strategy names (see util functions in Apoc.hs for function that prints strategies)
TODO 1)                                                               see point 6) in functional requirements (spec)
 
TODO 2) check strategy names(a and b), if either are illegal, print a list of strategy names and quit
TODO 2)                                if both legal, run strategies against one another (interactive)


TODO 3) Rob did this one for us... according to the spec the prompts should end in "B_" or "W_" 
        where '_' is either '2' or '4', not '1' or '2' so this might be a typo in his code


TODO 4) Implement an evasive strategy, where the chooser always favors a move to dodge a capture -- Graham 


TODO 5) For Main ============================================================

Whichever function we get user input in using ...

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
            (if one or both playTypes are PawnPlacement, we have to ask the choosers appropriately)
        move1 <- blackStrat state blackPlayType Black
        move2 <- whiteStrat state whitePlayType White
        ...  check moves  ....  -> double pass is a tie, game over  
        ... execute round ....
        ... print results ....
        playGame winCondition? newState blackStrat whiteStrat
    
playGame True state blackStrat whiteStrat = do
    do output for winning conditions satisfying IO() return type



just my idea that'll work well with what I already have written.




Notes : If a player passes on a pawn placement, it's a penalty.
      : If a player misses a capture, it's not a penalty (unfortunately our idea won't work)
      : If both players reach  2 penalty points in the same round, it's a tie
      : If both players pass the same round it's a tie and the game ends
      : Rob said he isn't sure if, in the case that two pawns are to be upgraded on the same turn, the
        output should be done in one step or two but we can write a script where that happens and he'll try it.
      : With user input, a PlayType of Passed must be recieved as an empty line without a comment,
        if the input has no leading integers ie "hey" the user should be reprompted. 


