get.diff.matrices <- function(p,q,N){
  # p, q are player serve & win probabilities
  # p = q for tennis games
  # N is minimum number of points needed to win
  # returns list of P, Q, R matrices
  # P from Arham's paper directly
  # Q and R make up 2 blocks of the extra points matrix
  # [ Q | R ]
  # [ 0 | I ] with 2 absorbing states instead of 1 used in the paper
  
  # all possible states
  all.states <- paste("(",rep(-N:N,2),",",c(rep("A",2*N+1),rep("B",2*N+1)),")",sep="")
  
  # probabilities
  p.a <- p
  q.a <- 1-p
  p.b <- 1-q
  q.b <- q
  
  # P matrix
  P <- matrix(0,nrow=(4*N-2),ncol=(4*N-2))
  rownames(P) <- all.states[3:(4*N)]
  colnames(P) <- all.states[3:(4*N)]
  for (i in 1:(2*N-2)){
    P[i,i+1] <- p.a
    P[i+2*N,i] <- p.b
    P[i,i+2*N] <- q.a
    P[i+2*N,i+2*N-1] <- q.b
  }
  P[2*N-1,2*N-1] <- 1
  P[2*N,2*N] <- 1
  
  # Q & R matrices
  Q <- matrix(0,nrow=4,ncol=4)
  R <- matrix(0,nrow=4,ncol=2)
  rownames(Q) <- c("(0,A)","(1,A)","(-1,B)","(0,B)")
  rownames(R) <- c("(0,A)","(1,A)","(-1,B)","(0,B)")
  colnames(Q) <- c("(0,A)","(1,A)","(-1,B)","(0,B)")
  colnames(R) <- c("(2,A)","(-2,B)")
  Q[1,2] <- p.a
  Q[1,3] <- q.a
  Q[2,4] <- q.a
  Q[3,1] <- p.b
  Q[4,2] <- p.b
  Q[4,3] <- q.b
  R[2,1] <- p.a
  R[3,2] <- q.b
  
  return(list("P"=P,"Q"=Q,"R"=R))
}

get.win.score.probs <- function(P,N){
  # returns 2 arrays of length N
  # arrays consisting of P_A(k) and P_B(k) defined in Arham's paper
  # matrices multiplied in 1-by-1 to:
  #   1. be able to extract the P_X(k) each step
  #   2. cut down on computation as first matrix only uses (0,A) row
  #   3. leave room for possible changing in matrix values mid calculation in the future
  
  a <- P["(0,A)",]
  # a no longer named num after matrix multiplication
  names <- names(a) # so storing now to assign after each iteration
  for (i in 1:(N-2)){
    a <- a %*% P
  }
  P_A <- rep(0,N)
  P_B <- rep(0,N)
  for (i in 1:N){
    a <- a %*% P
    names(a) <- names
    P_A[i] <- a[paste("(",N-i+1,",A)",sep="")]
    P_B[i] <- a[paste("(",i-N-1,",B)",sep="")]
  }
  return(list("P_A"=P_A,"P_B"=P_B))
}