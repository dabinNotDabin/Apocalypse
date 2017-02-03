# Apocalypse

TODO : TODO clauses can be found in the .hs files where they are relevant

Notes : I think my functions that determine play type are wrong but I have to ask Rob.
          Upgrade and PawnPlacement just look for any black pawn in row 0 or any white pawn in row 4.
          If the player is supposed to give a coordinate for pawn placement but they pass, is that pawn
          upgrade attempted again?, is it a penalty?


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
playGame False state blackStrat whiteStrat = do
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
