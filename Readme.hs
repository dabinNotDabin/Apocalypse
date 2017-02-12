# Apocalypse

Notes at the end may be important for some of these TODO clauses

TODO -8) I'm probably not gonna be up too early, It's 4:30 lol so I'm probably gonna sleep in and work on SENG. I got the random
         moves working to a degree. I couldn't get it to stop going into loops so I passed an int into playGame like Wynn said and after a
         certain # of turns it double passes and ties the game. In runStrategiesNormal you'll see this. I got it so that it goes into a
         loop less often and after 70 turns it just double passes and ties. If you wanna mess with it go for it.
         


TODO -7) I uploaded a bunch of .txt files that our program output when I ran it on his scripts. We have to compare them with the 
         *.out files that I also uploaded because his checker said 2/3 were correct.. I think it's in the passes one, it's upgrading
         the knight incorrectly.. Actually I figured it out, we pass all three tests in his mini checker now but some more would be
         good just to verify

TODO -6) Finish playGame (the one that's commented out) in Apoc.hs. I added to the old one quite a bit but left 
         it there so everything would still compile until we get the newer one finished.

TODO -5) Finish Function doWinResults in Apoc.hs -- it needs some supplemental functions to work properly.

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


        Notes : If a player passes on a pawn placement, it's a penalty.
      : If a player misses a capture, it's not a penalty (unfortunately our idea won't work)
      : If both players reach  2 penalty points in the same round, it's a tie
      : If both players pass the same round it's a tie and the game ends
      : Rob said he isn't sure if, in the case that two pawns are to be upgraded on the same turn, the
        output should be done in one step or two but we can write a script where that happens and he'll try it.
      : With user input, a PlayType of Passed must be recieved as an empty line without a comment,
        if the input has no leading integers ie "hey" the user should be reprompted. 
        
        
        
        
        
        
        
        

{-

playGame :: GameState -> Chooser -> Chooser -> Bool -> IO()
playGame False state blackStrat whiteStrat
    | (pawnUpgradeRequired (determinePlayType state Black) == True)  ||  (pawnUpgradeRequired (determinePlayType state White) == True)    = do
--      let newState = run upgrade  ....
         in print newState
            playGame newState blackStrat whiteStrat (checkForWin newState)
    | otherwise  = 
        let blackPlayType = determinePlayType state Black
            whitePlayType = determinePlayType state White
         in do newState <- roundExecute state blackPlayType whitePlayType
               playGame newState blackStrat whiteStrat (checkForWin newState)

--playGame True state blackStrat whiteStrat = do
--  output for winning conditions satisfying IO() return type

-}



        
        
        
        
        
        


