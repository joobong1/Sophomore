## ?????? ?ҷ?????
card1 <- read.csv("./card.csv")
card2 <- read.csv("./card2.csv")

play <- function(){
  
  ## ī?? ????
  shuffle <- function(card1) {
    random <- sample(1:20, size = 20)
    card1[random, ]
  }
  
  ## ī?? ?й?
  card3 <- shuffle(card1)
  a <- card3[c(1,5), ]
  b <- card3[c(3,7), ]
  
  ## ???? ????
  #PlayerA
  if(all(a[ ,3] %in% "??") & all(a[ ,2] %in% c(3,8))) {
    A<-"38????"
  } else if(all(a[ ,3] %in% "??") & all(a[ ,2] %in% c(1,8))) {
    A<-"18????"
  } else if(all(a[ ,3] %in% "??") & all(a[ ,2] %in% c(1,3))) {
    A<-"13????"
  } else if(a[1,2] == a[2,2]) {
    A <- paste(a[1,2],"??",sep = "")
  } else if(all(a[ ,2] %in% c(1,2))) {
    A <-"?˸?"
  } else if(all(a[ ,2] %in% c(1,4))) {
    A <-"????"
  } else if(all(a[ ,2] %in% c(1,9))) {
    A <-"????"
  } else if(all(a[ ,2] %in% c(1,10))) {
    A <-"????"
  } else if(all(a[ ,2] %in% c(4,10))) {
    A <-"????"
  } else if(all(a[ ,2] %in% c(4,6))) {
    A <-"????"
  } else if(all(a[ ,2] %in% c(4,7)) & all(a[ ,3] %in% c("??", "????"))) {
    A <-"????????"
  } else if(all(a[ ,2] %in% c(3,7)) ) {
    A <-"??????"
  } else if(all(a[ ,2] %in% c(4,9)) & all(a[ ,3] %in% c("??","??"))) {
    A <-"???ֱ???????"
  } else if(all(a[ ,2] %in% c(4,9))) {
    A <-"????"
  } else if(a[1,2] + a[2,2] > 10) {
    A <- paste(a[1,2] + a[2,2] - 10,"??",sep = "")
  } else if(a[1,2] + a[2,2] == 10 ) {
    A <-"????"
  } else {
    A <- paste(a[1,2] + a[2,2],"??",sep = "")
  }
  #PlayerB  
  if(all(b[ ,3] %in% "??") & all(b[ ,2] %in% c(3,8))) {
    B<-"38????"
  } else if(all(b[ ,3] %in% "??") & all(b[ ,2] %in% c(1,8))) {
    B<-"18????"
  } else if(all(b[ ,3] %in% "??") & all(b[ ,2] %in% c(1,3))) {
    B<-"13????"
  } else if(b[1,2] == b[2,2]) {
    B <- paste(b[1,2],"??",sep = "")
  } else if(all(b[ ,2] %in% c(1,2))) {
    B <-"?˸?"
  } else if(all(b[ ,2] %in% c(1,4))) {
    B <-"????"
  } else if(all(b[ ,2] %in% c(1,9))) {
    B <-"????"
  } else if(all(b[ ,2] %in% c(1,10))) {
    B <-"????"
  } else if(all(b[ ,2] %in% c(4,10))) {
    B <-"????"
  } else if(all(b[ ,2] %in% c(4,6))) {
    B <-"????"
  } else if(all(b[ ,2] %in% c(4,7)) & all(b[ ,3] %in% c("??","????"))) {
    B <-"????????"
  } else if(all(b[ ,2] %in% c(3,7)) ) {
    B <-"??????"
  } else if(all(b[ ,2] %in% c(4,9)) & all(b[ ,3] %in% c("??","??"))) {
    B <-"???ֱ???????"
  } else if(all(b[ ,2] %in% c(4,9))) {
    B <-"????"
  } else if(b[1,2] + b[2,2] > 10) {
    B <- paste(b[1,2] + b[2,2] - 10,"??" ,sep = "")
  } else if(b[1,2] + b[2,2] == 10 ) {
    B <-"????"
  } else {
    B <- paste(b[1,2] + b[2,2],"??",sep = "")
  }
  
  ## ???? ????
  if(A == card2[30,1] & B %in% card2[5:13,1]){
    scoreA <- 100 
    scoreB <- 1
  } else if(A == card2[31,1] & B %in% card2[4:29,1]){
    scoreA <- 1
    scoreB <- 1
  } else if(A == card2[32,1] & B %in% card2[14:29,1]){
    scoreA <- 1
    scoreB <- 1
  } else if(A == card2[33,1] & B %in% card2[2:3,1]){
    scoreA <- 100
    scoreB <- 1
  } else if(B == card2[30,1] & A %in% card2[5:13,1]){
    scoreA <- 1
    scoreB <- 100
  } else if(B == card2[31,1] & A %in% card2[4:29,1]){
    scoreA <- 1 
    scoreB <- 1
  } else if(B == card2[32,1] & A %in% card2[14:29,1]){
    scoreA <- 1
    scoreB <- 1 
  } else if(B == card2[33,1] & A %in% card2[2:3,1]){
    scoreA <- 1 
    scoreB <- 100
  } else {scoreA <- card2[card2[ ,1] == A, 2]
  scoreB <- card2[card2[ ,1] == B, 2]}
  
  ## ???? ????
  if(scoreA > scoreB){
    cat(" playerA",A,'\n',"playerB",B,'\n',"$PlayerA Wins!$")
  } else if(scoreA < scoreB){
    cat(" playerA",A,'\n',"playerB",B,'\n',"$PlayerB Wins!$")
  } else {
    cat(" playerA",A,'\n',"playerB",B,'\n',"Rematch!!")
  }
} 


## ???ӽ???

play()
play()
play()
