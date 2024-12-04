source("matrices/game_matrices.R")
source("matrices/game_matrices_pointwise.R")
source("matrices/tiebreak_matrices.R")
source("matrices/set_matrices.R")
source("matrices/match_matrices.R")


match.duration.nonpw <- function(p1,p2,mu,format=0){
  # p1(p2): probability of P1(P2) winning their service point
  # mu: minutes per point ratio estimate
  # format: acceptable values are
  #   0 - best of 3 sets, all tiebreaks to 7
  #     for all non-grand-slams and pre-2022 WTA US Open
  #   1 - best of 5 sets, all tiebreaks to 7
  #     for pre-2022 ATP US Open
  #   2 - best of 3 sets, deciding tiebreak to 10
  #     for all post-2022 WTA Grand Slams
  #   3 - best of 5 sets, deciding tiebreak to 10
  #     for all post-2022 ATP Grand Slams
  #   4 - best of 3 sets, deciding set advantage set
  #     for pre-2022 WTA Wimbledon, Roland Garros, Australian Open
  #   5 - best of 5 sets, deciding set advantage set
  #     for pre-2022 ATP Wimbledon, Roland Garros, Australian Open
  
  # Most probability calculation still needed as input for the matrices
  
  # p2g
  # prob and duration of P1, P2 serving games
  m.p2g.p1 <- point.to.game.matrices(p=p1)
  w.p2g.p1 <- solve(diag(18)-m.p2g.p1$Q)
  p1.g <- (w.p2g.p1%*%m.p2g.p1$R)[1,1]
  p1.t <- (w.p2g.p1%*%rep(1,18))[1]
  p1.t <- p1.t*mu
  
  m.p2g.p2 <- point.to.game.matrices(p=p2)
  w.p2g.p2 <- solve(diag(18)-m.p2g.p2$Q)
  p2.g <- (w.p2g.p2%*%m.p2g.p2$R)[1,1]
  p2.t <- (w.p2g.p2%*%rep(1,18))[1]
  p2.t <- p2.t*mu
  
  # similarly for tiebreak
  # unlike, prob functions, we will not discern who serves first
  # mathematically they yield the same result
  # 7-pt tiebreak needed for all, 10-pt only if format is 2 or 3
  m.tb.7 <- tb.to.7.matrices.new(p=p1,q=p2)
  w.tb.7 <- solve(diag(54)-m.tb.7$Q)
  tb.t <- (w.tb.7%*%rep(1,54))[1]
  tb.t <- tb.t*mu
  p1.tb.7 <- (w.tb.7%*%m.tb.7$R)[1,1]
  p2.tb.7 <- 1-p1.tb.7
  # 10-pt tiebreak
  if (format==2|format==3){
    m.tb.10 <- tb.to.10.matrices.new(p=p1,q=p2)
    w.tb.10 <- solve(diag(105)-m.tb.10$Q)
    tb.t.10 <- (w.tb.10%*%rep(1,105))[1]
    tb.t.10 <- tb.t.10*mu
    p1.tb.10 <- (w.tb.10%*%m.tb.10$R)[1,1]
    p2.tb.10 <- 1-p1.tb.10
  }
  
  # g2s
  # just like with tiebreaks, we have confirmed that 
  # who serves first doesn't affect duration or win prob
  m.g2s.tb <- game.to.set.matrices.tb(p=p1.g,q=p2.g,p.t=p1.tb.7)
  w.g2s.tb <- solve(diag(39)-m.g2s.tb$Q)
  p1.s <- (w.g2s.tb%*%m.g2s.tb$R)[1,1] # probability of p1 winning a set
  set7.t <- w.g2s.tb[1,]%*%c(p1.t,
                             rep(p2.t,2),
                             rep(p1.t,3),
                             rep(p2.t+1.5,4), # 90s changeover break
                             rep(p1.t,5),
                             rep(p2.t+1.5,6),
                             rep(p1.t,5),
                             rep(p2.t+1.5,4),
                             rep(p1.t,3),
                             rep(p2.t+1.5,2),
                             p1.t,p2.t+1.5,p2.t+1.5,tb.t)
  
  # g2s deciding sets
  # tiebreak to 10
  if (format==2|format==3){
    m.g2s.tb10 <- game.to.set.matrices.tb(p=p1.g,q=p2.g,p.t=p1.tb.10)
    w.g2s.tb10 <- solve(diag(39)-m.g2s.tb10$Q)
    p1.s.tb10 <- (w.g2s.tb10%*%m.g2s.tb10$R)[1,1] # probability of p1 winning deciding set
    set10.t <- w.g2s.tb10[1,]%*%c(p1.t,
                                  rep(p2.t,2),
                                  rep(p1.t,3),
                                  rep(p2.t+1.5,4),
                                  rep(p1.t,5),
                                  rep(p2.t+1.5,6),
                                  rep(p1.t,5),
                                  rep(p2.t+1.5,4),
                                  rep(p1.t,3),
                                  rep(p2.t+1.5,2),
                                  p1.t,p2.t+1.5,p2.t+1.5,tb.t.10)
  }
  # advantage sets
  if (format>3){
    m.g2s.adv <- game.to.set.matrices.adv(p=p1.g,q=p2.g)
    w.g2s.adv <- solve(diag(41)-m.g2s.adv$Q)
    p1.s.adv <- (w.g2s.adv%*%m.g2s.adv$R)[1,1]
    setadv.t <- w.g2s.adv[1,]%*%c(p1.t,
                                  rep(p2.t,2),
                                  rep(p1.t,3),
                                  rep(p2.t+1.5,4),
                                  rep(p1.t,5),
                                  rep(p2.t+1.5,6),
                                  rep(p1.t,5),
                                  rep(p2.t+1.5,4),
                                  rep(p1.t,3),
                                  rep(p2.t+1.5,2),
                                  p1.t,
                                  rep(p2.t+1.5,2),
                                  p1.t,
                                  rep(p2.t+1.5,2))
  }
  
  # s2m
  if (format==0){
    # 0 - best of 3 sets, all tiebreaks to 7
    m.s2m <- set.to.match.bo3.matrices(p1.s,p1.s)
    w.s2m <- solve(diag(6)-m.s2m$Q)
    t <- w.s2m[1,]%*%c(set7.t,2,set7.t,set7.t,2,set7.t)
  } else if (format==1){
    # 1 - best of 5 sets, all tiebreaks to 7
    m.s2m <- set.to.match.bo5.matrices(p1.s,p1.s)
    w.s2m <- solve(diag(16)-m.s2m$Q)
    t <- w.s2m[1,]%*%c(set7.t,2,set7.t,set7.t,2,2,2,set7.t,
                       set7.t,set7.t,2,2,set7.t,set7.t,2,set7.t)
  } else if (format==2){
    # 2 - best of 3 sets, deciding tiebreak to 10
    m.s2m <- set.to.match.bo3.matrices(p1.s,p1.s.tb10)
    w.s2m <- solve(diag(6)-m.s2m$Q)
    t <- w.s2m[1,]%*%c(set7.t,2,set7.t,set7.t,2,set10.t)
  } else if (format==3){
    # 3 - best of 5 sets, deciding tiebreak to 10
    m.s2m <- set.to.match.bo5.matrices(p1.s,p1.s.tb10)
    w.s2m <- solve(diag(16)-m.s2m$Q)
    t <- w.s2m[1,]%*%c(set7.t,2,set7.t,set7.t,2,2,2,set7.t,
                       set7.t,set7.t,2,2,set7.t,set7.t,2,set10.t)
  } else if (format==4){
    # 4 - best of 3 sets, deciding set advantage set
    m.s2m <- set.to.match.bo3.matrices(p1.s,p1.s.adv)
    w.s2m <- solve(diag(6)-m.s2m$Q)
    t <- w.s2m[1,]%*%c(set7.t,2,set7.t,set7.t,2,setadv.t)
  } else if (format==5){
    # 5 - best of 5 sets, deciding set advantage set
    m.s2m <- set.to.match.bo5.matrices(p1.s,p1.s.adv)
    w.s2m <- solve(diag(16)-m.s2m$Q)
    t <- w.s2m[1,]%*%c(set7.t,2,set7.t,set7.t,2,2,2,set7.t,
                       set7.t,set7.t,2,2,set7.t,set7.t,2,setadv.t)
  }
  return(t)
}