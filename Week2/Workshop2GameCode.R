# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
#A very simple game

#Game rules. In round 1, each player rolls two fair dice, and adds the resulting numbers together. 
#If one player has a lower score than all other players in the round, they are ``out`'', and` leave the game.
#Otherwise, no-one leaves the game.
#Either way, if more than one player remains in the game, move on to the next round.
#Continue playing rounds until all players but one have left the game. This player is the winnder.

#Very simple code for the very simple game.

#You can simulate rolling two dice and adding them together using the following code:

floor(runif(1,1,7))+floor(runif(1,1,7))

#Each player can use this code to simulate rolling two fair and adding the rolled numbers together. This is all you need to play the game.

#Less simple code for the very simple game.

#We can provide R with a list of the players in the game, and have R run the entire game for us. 
#We will use the "while" function to keep the game running until only one player remains.
#The function below requires a vector of the form e.g. c("Andrea", "Andy", "Hyeyoung", "Ric") to be entered as the variable.

game1<-function(x){
playercount<-length(x)
while(playercount>1){
    rolls<-rep(0,length(x))
    for(i in 1:length(x)){
        rolls[i]<-floor(runif(1,1,7))+floor(runif(1,1,7))
        }
    loseroll<-min(rolls)
    number<-length(which(rolls==loseroll))
    if(number==1){
      x<-x[-which(rolls==loseroll)]
      playercount<-playercount-1
    }
    }
return(x)
}

#The above doesn't tell us what's going on in the game, though. A little more work gives us:

game2<-function(x){
  playercount<-length(x)
  while(playercount>1){
    rolls<-rep(0,length(x))
    for(i in 1:length(x)){
      rolls[i]<-floor(runif(1,1,7))+floor(runif(1,1,7))
    }
    loseroll<-min(rolls)
    number<-length(which(rolls==loseroll))
    if(number==1){
      print(paste('Losing roll:',loseroll))
      print(paste('We remove',x[which(rolls==loseroll)]))
      x<-x[-which(rolls==loseroll)]
      playercount<-playercount-1
    }
  }
  return(x)
}

#We could even do

game3<-function(x){
  playercount<-length(x)
  round<-1
  print(paste('Initial player:',x))
  while(playercount>1){
    print(paste("Round:",round))
    rolls<-rep(0,length(x))
    for(i in 1:length(x)){
      rolls[i]<-floor(runif(1,1,7))+floor(runif(1,1,7))
    }
    print(paste('roll for',x[1:length(x)],'is',rolls[1:length(x)]))
    loseroll<-min(rolls)
    number<-length(which(rolls==loseroll))
    if(number==1){
      print(paste('Losing roll:',loseroll))
      print(paste('We remove',x[which(rolls==loseroll)]))
      x<-x[-which(rolls==loseroll)]
      playercount<-playercount-1
    }
    else(print("No-one is removed"))
    round<-round+1
  }
  return(paste(x,'wins!'))
}


