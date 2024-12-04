point.to.game.matrices <- function(p){
  # A is the server of the game
  # p: probability A wins their serving game
  # q: probability B wins their serving game
  
  Q <- matrix(0,nrow=18,ncol=18)
  p.idx <- rbind(c(1,2),c(2,4),c(3,5),c(4,7),c(5,8),c(6,9),c(8,11),
                 c(9,12),c(10,13),c(12,14),c(13,15),c(15,16),c(16,17),c(18,16))
  cp.idx <- rbind(c(1,3),c(2,5),c(3,6),c(4,8),c(5,9),c(6,10),c(7,11),
                  c(8,12),c(9,13),c(11,14),c(12,15),c(14,16),c(16,18),c(17,16))
  Q[p.idx] <- p
  Q[cp.idx] <- 1-p
  
  R <- matrix(0,nrow=18,ncol=2)
  R[7,1] <- p
  R[10,2] <- 1-p
  R[11,1] <- p
  R[13,2] <- 1-p
  R[14,1] <- p
  R[15,2] <- 1-p
  R[17,1] <- p
  R[18,2] <- 1-p
  
  return(list("Q"=Q,"R"=R))
}