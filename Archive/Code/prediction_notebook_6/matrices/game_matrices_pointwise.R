point.to.game.matrices.pw <- function(p){
  # A is the server of the game
  # p: probability A wins their serving game
  
  # KEY MODIFICATION from original:
  # p now a vector of length 18 for the 18 states
  
  Q <- matrix(0,nrow=18,ncol=18)
  Q[1,2] <- p[1]
  Q[1,3] <- 1-p[1]
  Q[2,4] <- p[2]
  Q[2,5] <- 1-p[2]
  Q[3,5] <- p[3]
  Q[3,6] <- 1-p[3]
  Q[4,7] <- p[4]
  Q[4,8] <- 1-p[4]
  Q[5,8] <- p[5]
  Q[5,9] <- 1-p[5]
  Q[6,9] <- p[6]
  Q[6,10] <- 1-p[6]
  Q[7,11] <- 1-p[7]
  Q[8,11] <- p[8]
  Q[8,12] <- 1-p[8]
  Q[9,12] <- p[9]
  Q[9,13] <- 1-p[9]
  Q[10,13] <- p[10]
  Q[11,14] <- 1-p[11]
  Q[12,14] <- p[12]
  Q[12,15] <- 1-p[12]
  Q[13,15] <- p[13]
  Q[14,16] <- 1-p[14]
  Q[15,16] <- p[15]
  Q[16,17] <- p[16]
  Q[16,18] <- 1-p[16]
  Q[17,16] <- 1-p[17]
  Q[18,16] <- p[18]
  
  R <- matrix(0,nrow=18,ncol=2)
  R[7,1] <- p[7]
  R[10,2] <- 1-p[10]
  R[11,1] <- p[11]
  R[13,2] <- 1-p[13]
  R[14,1] <- p[14]
  R[15,2] <- 1-p[15]
  R[17,1] <- p[17]
  R[18,2] <- 1-p[18]
  
  return(list("Q"=Q,"R"=R))
}