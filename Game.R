## µ•¿Ã≈Õ ∫“∑Øø¿±‚
card1 <- read.csv("./card.csv")
card2 <- read.csv("./card2.csv")

play <- function(){
  
  ## ƒ´µÂ ºØ±‚
  shuffle <- function(card1) {
    random <- sample(1:20, size = 20)
    card1[random, ]
  }
  
  ## ƒ´µÂ ∫–πË
  card3 <- shuffle(card1)
  a <- card3[c(1,5), ]
  b <- card3[c(3,7), ]
  
  ## ¡∑∫∏ º≥¡§
  #PlayerA
  if(all(a[ ,3] %in% "±§") & all(a[ ,2] %in% c(3,8))) {
    A<-"38±§∂Ø"
  } else if(all(a[ ,3] %in% "±§") & all(a[ ,2] %in% c(1,8))) {
    A<-"18±§∂Ø"
  } else if(all(a[ ,3] %in% "±§") & all(a[ ,2] %in% c(1,3))) {
    A<-"13±§∂Ø"
  } else if(a[1,2] == a[2,2]) {
    A <- paste(a[1,2],"∂Ø",sep = "")
  } else if(all(a[ ,2] %in% c(1,2))) {
    A <-"æÀ∏Æ"
  } else if(all(a[ ,2] %in% c(1,4))) {
    A <-"µ∂ªÁ"
  } else if(all(a[ ,2] %in% c(1,9))) {
    A <-"±∏ªÊ"
  } else if(all(a[ ,2] %in% c(1,10))) {
    A <-"¿ÂªÊ"
  } else if(all(a[ ,2] %in% c(4,10))) {
    A <-"¿ÂªÁ"
  } else if(all(a[ ,2] %in% c(4,6))) {
    A <-"ºº∑˙"
  } else if(all(a[ ,2] %in% c(4,7)) & all(a[ ,3] %in% c("ªı", "¡¸Ω¬"))) {
    A <-"æœ«‡æÓªÁ"
  } else if(all(a[ ,2] %in% c(3,7)) ) {
    A <-"∂Ø¿‚¿Ã"
  } else if(all(a[ ,2] %in% c(4,9)) & all(a[ ,3] %in% c("ªı","≤…"))) {
    A <-"∏€≈÷±∏∏Æ±∏ªÁ"
  } else if(all(a[ ,2] %in% c(4,9))) {
    A <-"±∏ªÁ"
  } else if(a[1,2] + a[2,2] > 10) {
    A <- paste(a[1,2] + a[2,2] - 10,"≤˝",sep = "")
  } else if(a[1,2] + a[2,2] == 10 ) {
    A <-"∏¡≈Î"
  } else {
    A <- paste(a[1,2] + a[2,2],"≤˝",sep = "")
  }
  #PlayerB  
  if(all(b[ ,3] %in% "±§") & all(b[ ,2] %in% c(3,8))) {
    B<-"38±§∂Ø"
  } else if(all(b[ ,3] %in% "±§") & all(b[ ,2] %in% c(1,8))) {
    B<-"18±§∂Ø"
  } else if(all(b[ ,3] %in% "±§") & all(b[ ,2] %in% c(1,3))) {
    B<-"13±§∂Ø"
  } else if(b[1,2] == b[2,2]) {
    B <- paste(b[1,2],"∂Ø",sep = "")
  } else if(all(b[ ,2] %in% c(1,2))) {
    B <-"æÀ∏Æ"
  } else if(all(b[ ,2] %in% c(1,4))) {
    B <-"µ∂ªÁ"
  } else if(all(b[ ,2] %in% c(1,9))) {
    B <-"±∏ªÊ"
  } else if(all(b[ ,2] %in% c(1,10))) {
    B <-"¿ÂªÊ"
  } else if(all(b[ ,2] %in% c(4,10))) {
    B <-"¿ÂªÁ"
  } else if(all(b[ ,2] %in% c(4,6))) {
    B <-"ºº∑˙"
  } else if(all(b[ ,2] %in% c(4,7)) & all(b[ ,3] %in% c("ªı","¡¸Ω¬"))) {
    B <-"æœ«‡æÓªÁ"
  } else if(all(b[ ,2] %in% c(3,7)) ) {
    B <-"∂Ø¿‚¿Ã"
  } else if(all(b[ ,2] %in% c(4,9)) & all(b[ ,3] %in% c("ªı","≤…"))) {
    B <-"∏€≈÷±∏∏Æ±∏ªÁ"
  } else if(all(b[ ,2] %in% c(4,9))) {
    B <-"±∏ªÁ"
  } else if(b[1,2] + b[2,2] > 10) {
    B <- paste(b[1,2] + b[2,2] - 10,"≤˝" ,sep = "")
  } else if(b[1,2] + b[2,2] == 10 ) {
    B <-"∏¡≈Î"
  } else {
    B <- paste(b[1,2] + b[2,2],"≤˝",sep = "")
  }
  
  ## ¡°ºˆ º≥¡§
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
  
  ## Ω¬∆– ø©∫Œ
  if(scoreA > scoreB){
    cat(" playerA",A,'\n',"playerB",B,'\n',"$PlayerA Wins!$")
  } else if(scoreA < scoreB){
    cat(" playerA",A,'\n',"playerB",B,'\n',"$PlayerB Wins!$")
  } else {
    cat(" playerA",A,'\n',"playerB",B,'\n',"Rematch!!")
  }
} 


## ∞‘¿”Ω««‡

play()
play()
play()
