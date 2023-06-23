set.to.match.of3 <- function(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                             p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                             p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d){
  # parameters format: x.y.Z
  #   x either p or q, p/q being probability A/B wins the set
  #     where A is the player who serves the 1st game of the 1st set
  #   y either b or h, b if the winner won by breaking their opponents serve,
  #     h if the winner won by holding their serve
  #   Z either A or B, indicating which player served the 1st game of the current set
  #   d at the end are the same corresponding probabilities, but for deciding sets
  
  Q <- matrix(0,nrow=7,ncol=7)
  Q[1,2] <- p.b.A
  Q[1,3] <- p.h.A
  Q[1,4] <- q.h.A
  Q[1,5] <- q.b.A
  Q[2,6] <- q.h.A
  Q[2,7] <- q.b.A
  Q[3,6] <- q.h.B
  Q[3,7] <- q.b.B
  Q[4,6] <- p.b.A
  Q[4,7] <- p.h.A
  Q[5,6] <- p.b.B
  Q[5,7] <- p.h.B
  
  R <- matrix(0,nrow=7,ncol=2)
  R[2,1] <- p.b.A+p.h.A
  R[3,1] <- p.b.B+p.h.B
  R[4,2] <- q.b.A+q.h.A
  R[5,2] <- q.b.B+q.h.B
  R[6,1] <- p.b.A.d+p.h.A.d
  R[6,2] <- q.b.A.d+q.h.A.d
  R[7,1] <- p.b.B.d+p.h.B.d
  R[7,2] <- q.b.B.d+q.h.B.d
  
  return(list("Q"=Q,"R"=R))
}

set.to.match.of5 <- function(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                             p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                             p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d){
  # parameters format: x.y.Z
  #   x either p or q, p/q being probability A/B wins the set
  #     where A is the player who serves the 1st game of the 1st set
  #   y either b or h, b if the winner won by breaking their opponents serve,
  #     h if the winner won by holding their serve
  #   Z either A or B, indicating which player served the 1st game of the current set
  #   d at the end are the same corresponding probabilities, but for deciding sets
  
  Q <- matrix(0,nrow=17,ncol=17)
  pba.idx <- rbind(c(1,2),c(2,6),c(4,8),c(8,12),c(10,14),c(14,16))
  Q[pba.idx] <- p.b.A
  pha.idx <- rbind(c(1,3),c(2,7),c(4,9),c(8,13),c(10,15),c(14,17))
  Q[pha.idx] <- p.h.A
  qha.idx <- rbind(c(1,4),c(2,8),c(4,10),c(6,12),c(8,14),c(12,16))
  Q[qha.idx] <- q.h.A
  qba.idx <- rbind(c(1,5),c(2,9),c(4,11),c(6,13),c(8,15),c(12,17))
  Q[qba.idx] <- q.b.A
  pbb.idx <- rbind(c(3,6),c(5,8),c(9,12),c(11,14),c(15,16))
  Q[pbb.idx] <- p.b.B
  phb.idx <- rbind(c(3,7),c(5,9),c(9,13),c(11,15),c(15,17))
  Q[phb.idx] <- p.h.B
  qhb.idx <- rbind(c(3,8),c(5,10),c(7,12),c(9,14),c(13,16))
  Q[qhb.idx] <- q.h.B
  qbb.idx <- rbind(c(3,9),c(5,11),c(7,13),c(9,15),c(13,17))
  Q[qbb.idx] <- q.b.B
  
  R <- matrix(0,nrow=17,ncol=2)
  R[6,1] <- p.b.A+p.h.A
  R[7,1] <- p.b.B+p.h.B
  R[10,2] <- q.b.A+q.h.A
  R[11,2] <- q.b.B+q.h.B
  R[12,1] <- p.b.A+p.h.A
  R[13,1] <- p.b.B+p.h.B
  R[14,2] <- q.b.A+q.h.A
  R[15,2] <- q.b.B+q.h.B
  R[16,1] <- p.b.A.d+p.h.A.d
  R[16,2] <- q.b.A.d+q.h.A.d
  R[17,1] <- p.b.B.d+p.h.B.d
  R[17,2] <- q.b.B.d+q.h.B.d
  
  return(list("Q"=Q,"R"=R))
}