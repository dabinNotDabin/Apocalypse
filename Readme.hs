# Apocalypse

TODO : TODO clauses can be found in the .hs files where they are relevant


Whichever function we get input in ... 
ie;
    move1 <- blackStrategy state Normal Black
    move2 <- whiteStrategy state Normal White
    
... must have an IO (...) return type, can be IO GameState, IO(), etc.
This is because the operator <- must be within a do block

We can create a playGame function that is like our highest order function but it has to return some IO type.
If it takes two chooser types, it can get the moves from the strategies as shown above since it's an IO type
and the game always starts with;
  state    ==   initBoard and 
  PlayType ==   Normal (for both players)
 

   -- we can have it print all results as it goes, calling itself recursively (if no win state detected)
   -- on finding a win state, it can print out the results of the match satisfying the IO() return type
   

    
playGame :: Bool -> GameState -> Chooser -> Chooser -> IO()
playGame False initBoard blackStrat whiteStrat = do
    move1 <- blackStrat initBoard Normal Black
    move2 <- whiteStrat initBoard Normal White
    .... execute round using roundSelector
    .... print results
    playGame winCondition? newState blackStrat whiteStrat
    
playGame False state blackStrat whiteStrat = do
    let blackPlayType = determinePlayType
        whitePlayType = determinePlayType
    move1 <- blackStrat state blackPlayType Black
    move2 <- whiteStrat state whitePlayType White
    ... execute round using roundSelector
    ... print results
    playGame winCondition? newState blackStrat whiteStrat
    
playGame True state blackStrat whiteStrat = do
    do output for winning conditions satisfying IO() return type


just my idea that'll work well with what I already have written.
