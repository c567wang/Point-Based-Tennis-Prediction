game.to.set.matrices <- function(p,q){
  # A is the server of the first game of the set
  # p: probability A wins their serving game
  # q: probability B wins their serving game
  
  Q <- matrix(0,nrow=38,ncol=38)
  p.idx <- rbind(c(1,2),c(4,7),c(5,8),c(6,9),c(11,16),c(12,17),c(13,18),
                 c(14,19),c(15,20),c(23,27),c(24,28),c(25,29),c(26,30),c(32,34),
                 c(33,35),c(36,37))
  cp.idx <- rbind(c(1,3),c(4,8),c(5,9),c(6,10),c(11,17),c(12,18),c(13,19),
                  c(14,20),c(15,21),c(22,27),c(23,28),c(24,29),c(25,30),c(31,34),
                  c(32,35),c(36,38))
  q.idx <- rbind(c(2,5),c(3,6),c(7,12),c(8,13),c(9,14),c(10,15),c(16,22),
                 c(17,23),c(18,24),c(19,25),c(20,26),c(27,31),c(28,32),c(29,33),
                 c(34,36))
  cq.idx <- rbind(c(2,4),c(3,5),c(7,11),c(8,12),c(9,13),c(10,14),c(17,22),
                  c(18,23),c(19,24),c(20,25),c(21,26),c(28,31),c(29,32),c(30,33),
                  c(35,36))
  Q[p.idx] <- p
  Q[cp.idx] <- 1-p
  Q[q.idx] <- q
  Q[cq.idx] <- 1-q
  
  R <- matrix(0,nrow=38,ncol=5)
  R[16,3] <- 1-q
  R[21,4] <- q
  R[22,2] <- p
  R[26,5] <- 1-p
  R[27,3] <- 1-q
  R[30,4] <- q
  R[31,2] <- p
  R[33,5] <- 1-p
  R[34,3] <- 1-q
  R[35,4] <- q
  R[37,1] <- q
  R[37,3] <- 1-q
  R[38,1] <- 1-q
  R[38,4] <- q
  
  return(list("Q"=Q,"R"=R))
}

special.set.matrices <- function(p,q){
  # used for advantage sets and the 12-12 sets that 
  # Wimbledon used for 2019 and 2021
  
  Q <- matrix(0,nrow=3,ncol=3)
  Q[1,2] <- p
  Q[1,3] <- 1-p
  Q[2,1] <- q
  Q[3,1] <- 1-q
  
  R <- matrix(0,nrow=3,ncol=2)
  R[2,1] <- 1-q
  R[3,2] <- q
  
  P <- cbind(rbind(Q,matrix(0,nrow=2,ncol=3)),
             rbind(R,diag(2)))
  
  return(list("Q"=Q,"R"=R,"P"=P))
}
