source("matrices/game_matrices.R")
source("matrices/game_matrices_pointwise.R")
source("matrices/tiebreak_matrices.R")
source("matrices/set_matrices.R")
source("matrices/match_matrices.R")
# install.packages("expm")
library(expm)

match.win.prob.nonpw <- function(p1,p2,format=0){
  # function accounts for match formats from 2011 onwards!
  # under the iid assumptions, calculates player 1 (P1)'s probability of winning a match
  # with player 2 (P2), where P1 is the player who serves the first game of the first set
  # input:
  #   p1(p2): probability of P1(P2) winning a point in their serving game
  #   format: acceptable values are 0,1,2
  #     0 - best of 3 sets, all tiebreaks to 7
  #       for all non-grand-slams, WTA USO up to 2021
  #     1 - best of 5 sets, all tiebreaks to 7
  #       for ATP USO up to 2021
  #     2 - best of 3 sets, deciding tiebreak to 10
  #       for WTA all 2022 grand-slams + WTA AUS 2019-2021
  #     3 - best of 5 sets, deciding tiebreak to 10
  #       for ATP all 2022 grand-slams, ATP AUS 2019-2021
  #     4 - best of 3 sets, deciding set advantage set
  #       for WTA WBD up to 2018, WTA RG up to 2021, WTA AUS up to 2018
  #     5 - best of 5 sets, deciding set advantage set
  #       for ATP WBD up to 2018, ATP RG up to 2021, ATP AUS up to 2018
  #     6 - best of 3 sets, deciding set to 7 only if set is tied at 12-12
  #       for WTA WBD 2019 & 2021
  #     7 - best of 5 sets, deciding set to 7 only if set is tied at 12-12
  #       for ATP WBD 2019 & 2021
  #   p: probability model gives of P1 winning the match
  
  # get probs of P1,P2 holding their serve
  m.p2g.p1 <- point.to.game.matrices(p=p1)
  p1.g <- (solve(diag(18)-m.p2g.p1$Q)%*%m.p2g.p1$R)[1,1]
  m.p2g.p2 <- point.to.game.matrices(p=p2)
  p2.g <- (solve(diag(18)-m.p2g.p2$Q)%*%m.p2g.p2$R)[1,1]
  
  # get probs of P1,P2 winning sets
  # in this block of code, probs correspond to sets where P1 serves the 1st game 
  m.g2s.p1 <- game.to.set.matrices(p=p1.g,q=p2.g)
  m.g2s.p1.w <- (solve(diag(38)-m.g2s.p1$Q)%*%m.g2s.p1$R)[1,]
  p.s1.t <- m.g2s.p1.w[1] # prob of going to tiebreak in such a set
  p1.s1.h <- m.g2s.p1.w[2] # prob of P1 winning by holding in such a set
  p1.s1.b <- m.g2s.p1.w[3] # prob of P1 winning by breaking P2's serve
  p2.s1.h <- m.g2s.p1.w[4] # prob of P2 winning by holding in such a set
  p2.s1.b <- m.g2s.p1.w[5] # prob of P2 winning by breaking P1's serve
  
  # in this block of code, probs correspond to sets where P2 serves the 1st game 
  m.g2s.p2 <- game.to.set.matrices(p=p2.g,q=p1.g)
  m.g2s.p2.w <- (solve(diag(38)-m.g2s.p2$Q)%*%m.g2s.p2$R)[1,]
  p.s2.t <- m.g2s.p2.w[1] # prob of going to tiebreak in such a set
  p2.s2.h <- m.g2s.p2.w[2] # prob of P2 winning by holding in such a set
  p2.s2.b <- m.g2s.p2.w[3] # prob of P2 winning by breaking P1's serve
  p1.s2.h <- m.g2s.p2.w[4] # prob of P1 winning by holding in such a set
  p1.s2.b <- m.g2s.p2.w[5] # prob of P1 winning by breaking P2's serve
  
  # get probs for players winning tiebreak games
  m.tb.p1 <- tb.to.7.matrices(p=p1,q=p2) # to 7, P1 serves first
  m.tb.p1.w <- (solve(diag(48)-m.tb.p1$Q)%*%m.tb.p1$R)[1,]
  p.tb1.to7 <- m.tb.p1.w[1] # prob of the tiebreak needing more than 12 pts played
  p1.tb1.to7 <- m.tb.p1.w[2] # prob of P1 winning tiebreak when serving first
  p2.tb1.to7 <- m.tb.p1.w[3] # prob of P2 winning tiebreak when receiving first
  
  m.tb.p2 <- tb.to.7.matrices(p=p2,q=p1) # to 7, P2 serves first
  m.tb.p2.w <- (solve(diag(48)-m.tb.p2$Q)%*%m.tb.p2$R)[1,]
  p.tb2.to7 <- m.tb.p2.w[1] # prob of the tiebreak needing more than 12 pts played
  p2.tb2.to7 <- m.tb.p2.w[2] # prob of P2 winning tiebreak when serving first
  p1.tb2.to7 <- m.tb.p2.w[3] # prob of P1 winning tiebreak when receiving first
  
  if (format==2|format==3){ # will need to 10 prob if format is 2 or 3
    m.tbd.p1 <- tb.to.10.matrices(p=p1,q=p2) # to 10, P1 serves first
    m.tbd.p1.w <- (solve(diag(99)-m.tbd.p1$Q)%*%m.tbd.p1$R)[1,]
    p.tb1.to10 <- m.tbd.p1.w[1] # prob of the tiebreak needing more than 18 pts played
    p1.tb1.to10 <- m.tbd.p1.w[2] # prob of P1 winning tiebreak when serving first
    p2.tb1.to10 <- m.tbd.p1.w[3] # prob of P2 winning tiebreak when receiving first
    
    m.tbd.p2 <- tb.to.10.matrices(p=p2,q=p1) # to 10, P2 serves first
    m.tbd.p2.w <- (solve(diag(99)-m.tbd.p2$Q)%*%m.tbd.p2$R)[1,]
    p.tb2.to10 <- m.tbd.p2.w[1] # prob of the tiebreak needing more than 18 pts played
    p2.tb2.to10 <- m.tbd.p2.w[2] # prob of P2 winning tiebreak when serving first
    p1.tb2.to10 <- m.tbd.p2.w[3] # prob of P1 winning tiebreak when receiving first
  }
  
  # probs for if a tiebreak needs more than 7(10) points to win
  # for non-deciding sets (to 7), chain starts at (T,A2) which is the first row
  # for deciding sets (to 10), chain starts at (T,B2) which is the second row
  # results in this block for when P1 served first in the tiebreak
  # note the player who serves first in the tiebreak served first in the set
  m.tbe.p1 <- tb.extra.matrices(p=p1,q=p2)
  m.tbe.p1.w <- solve(diag(6)-m.tbe.p1$Q)%*%m.tbe.p1$R
  p1.tbe.to7 <- m.tbe.p1.w[1,1] # P1 win prob once extra pts are played in the tiebreak
  p1.tbe.to10 <- m.tbe.p1.w[2,1] # for P2's probs, need only subtract this from 1
  
  # results in this block for when P2 served first in the tiebreak
  m.tbe.p2 <- tb.extra.matrices(p=p2,q=p1)
  m.tbe.p2.w <- solve(diag(6)-m.tbe.p2$Q)%*%m.tbe.p2$R
  p2.tbe.to7 <- m.tbe.p2.w[1,1] # P2 win prob once extra pts are played in the tiebreak
  p2.tbe.to10 <- m.tbe.p2.w[2,1] # for P1's probs, need only subtract this from 1
  
  # now that the individual pieces are established, start calculating overall set
  # winning probabilities to plug into set.to.match functions
  # since P1 was designated as serving the 1st game of the entire match
  # A(B) will be synonymous with P1(P2) in variable naming from here on in
  p.b.A <- p1.s1.b
  p.h.A <- p1.s1.h+p.s1.t*(p1.tb1.to7+p.tb1.to7*p1.tbe.to7)
  q.b.A <- p2.s1.b+p.s1.t*(p2.tb1.to7+p.tb1.to7*(1-p1.tbe.to7))
  q.h.A <- p2.s1.h
  p.b.B <- p1.s2.b+p.s2.t*(p1.tb2.to7+p.tb2.to7*(1-p2.tbe.to7))
  p.h.B <- p1.s2.h
  q.b.B <- p2.s2.b
  q.h.B <- p2.s2.h+p.s2.t*(p2.tb2.to7+p.tb2.to7*p2.tbe.to7)
  
  # sacrificing some succinctness for readability below so cases are clear
  if (format==0){
    # 0 - best of 3 sets, all tiebreaks to 7
    # for all non-grand-slams, WTA USO up to 2021
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==1) {
    # 1 - best of 5 sets, all tiebreaks to 7
    # for ATP USO up to 2021
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==2) {
    # 2 - best of 3 sets, deciding tiebreak to 10
    # for WTA all 2022 grand-slams + WTA AUS 2019-2021
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.tb1.to10+p.tb1.to10*p1.tbe.to10)
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.tb1.to10+p.tb1.to10*(1-p1.tbe.to10))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.tb2.to10+p.tb2.to10*(1-p2.tbe.to10))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.tb2.to10+p.tb2.to10*p2.tbe.to10)
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==3) {
    # 3 - best of 5 sets, deciding tiebreak to 10
    # for ATP all 2022 grand-slams, ATP AUS 2019-2021
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.tb1.to10+p.tb1.to10*p1.tbe.to10)
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.tb1.to10+p.tb1.to10*(1-p1.tbe.to10))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.tb2.to10+p.tb2.to10*(1-p2.tbe.to10))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.tb2.to10+p.tb2.to10*p2.tbe.to10)
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==4) {
    # 4 - best of 3 sets, deciding set advantage set
    # for WTA WBD up to 2018, WTA RG up to 2021, WTA AUS up to 2018
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # (this means P1 also served the first game of the set)
    m.advse.p1 <- special.set.matrices(p1.g,p2.g)
    p1.advse <- (solve(diag(3)-m.advse.p1$Q)%*%m.advse.p1$R)[1,1] 
    # take complement wrt 1 for prob of P2 winning advantage set once extra games
    # and when P1 serves the first extra game (the 13th game of the set)
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # similar to explanation above, complement wrt 1 will be used as well
    m.advse.p2 <- special.set.matrices(p2.g,p1.g)
    p2.advse <- (solve(diag(3)-m.advse.p2$Q)%*%m.advse.p2$R)[1,1]
    
    # doesn't really matter which part to add the extra game probs to
    # since they're summed in the final set-to-match function
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*p1.advse
    q.b.A.d <- p2.s1.b+p.s1.t*(1-p1.advse)
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(1-p2.advse)
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*p2.advse
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==5) {
    # 5 - best of 5 sets, deciding set advantage set
    # for ATP WBD up to 2018, ATP RG up to 2021, ATP AUS up to 2018
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # (this means P1 also served the first game of the set)
    m.advse.p1 <- special.set.matrices(p1.g,p2.g)
    p1.advse <- (solve(diag(3)-m.advse.p1$Q)%*%m.advse.p1$R)[1,1] 
    # take complement wrt 1 for prob of P2 winning advantage set once extra games
    # and when P1 serves the first extra game (the 13th game of the set)
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # similar to explanation above, complement wrt 1 will be used as well
    m.advse.p2 <- special.set.matrices(p2.g,p1.g)
    p2.advse <- (solve(diag(3)-m.advse.p2$Q)%*%m.advse.p2$R)[1,1]
    
    # doesn't really matter which part to add the extra game probs to
    # since they're summed in the final set-to-match function
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*p1.advse
    q.b.A.d <- p2.s1.b+p.s1.t*(1-p1.advse)
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(1-p2.advse)
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*p2.advse
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==6) {
    # 6 - best of 3 sets, deciding set to 7 only if set is tied at 12-12
    # for WTA WBD 2019 & 2021
    
    # following two code blocks for probs re: deciding set goes to extra games
    # the first block's probs are for when P1 serves the first extra game
    m.wbdsp.p1 <- special.set.matrices(p1.g,p2.g)$P #wbdsp - wimbledon special
    m.wbdsp.p1.w <- (m.wbdsp.p1%^%12)[1,] # expm package needs to be installed
    p1.wbdsp.s1 <- m.wbdsp.p1.w[4] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s1 <- m.wbdsp.p1.w[5] # prob P2 wins in same scenario
    t.wbdsp.s1 <- m.wbdsp.p1.w[1] # prob goes to 12-12 and tiebreak happens
    
    # the second block's probs are for when P2 serves the first extra game
    m.wbdsp.p2 <- special.set.matrices(p2.g,p1.g)$P
    m.wbdsp.p2.w <- (m.wbdsp.p2%^%12)[1,]
    p1.wbdsp.s2 <- m.wbdsp.p2.w[5] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s2 <- m.wbdsp.p2.w[4] # prob P2 wins in same scenario
    t.wbdsp.s2 <- m.wbdsp.p2.w[1] # prob goes to 12-12 for tiebreak
    
    p.b.A.d <- p1.s1.b 
    # in 12-12 to7 tiebreak, same player serves first in the tiebreak
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.wbdsp.s1+t.wbdsp.s1*(p1.tb1.to7+p.tb1.to7*p1.tbe.to7))
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.wbdsp.s1+t.wbdsp.s1*(p2.tb1.to7+p.tb1.to7*(1-p1.tbe.to7)))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.wbdsp.s2+t.wbdsp.s2*(p1.tb2.to7+p.tb2.to7*(1-p2.tbe.to7)))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.wbdsp.s2+t.wbdsp.s2*(p2.tb2.to7+p.tb2.to7*p2.tbe.to7))
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==7) {
    # 7 - best of 5 sets, deciding set to 7 only if set is tied at 12-12
    # for ATP WBD 2019 & 2021
    
    # following two code blocks for probs re: deciding set goes to extra games
    # the first block's probs are for when P1 serves the first extra game
    m.wbdsp.p1 <- special.set.matrices(p1.g,p2.g)$P #wbdsp - wimbledon special
    m.wbdsp.p1.w <- (m.wbdsp.p1%^%12)[1,] # expm package needs to be installed
    p1.wbdsp.s1 <- m.wbdsp.p1.w[4] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s1 <- m.wbdsp.p1.w[5] # prob P2 wins in same scenario
    t.wbdsp.s1 <- m.wbdsp.p1.w[1] # prob goes to 12-12 and tiebreak happens
    
    # the second block's probs are for when P2 serves the first extra game
    m.wbdsp.p2 <- special.set.matrices(p2.g,p1.g)$P
    m.wbdsp.p2.w <- (m.wbdsp.p2%^%12)[1,]
    p1.wbdsp.s2 <- m.wbdsp.p2.w[5] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s2 <- m.wbdsp.p2.w[4] # prob P2 wins in same scenario
    t.wbdsp.s2 <- m.wbdsp.p2.w[1] # prob goes to 12-12 for tiebreak
    
    p.b.A.d <- p1.s1.b 
    # in 12-12 to7 tiebreak, same player serves first in the tiebreak
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.wbdsp.s1+t.wbdsp.s1*(p1.tb1.to7+p.tb1.to7*p1.tbe.to7))
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.wbdsp.s1+t.wbdsp.s1*(p2.tb1.to7+p.tb1.to7*(1-p1.tbe.to7)))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.wbdsp.s2+t.wbdsp.s2*(p1.tb2.to7+p.tb2.to7*(1-p2.tbe.to7)))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.wbdsp.s2+t.wbdsp.s2*(p2.tb2.to7+p.tb2.to7*p2.tbe.to7))
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else {
    p <- -1 # invalid format value
  }
  return(p)
}

match.win.prob.pw <- function(p1,p2,format=0){
  # function accounts for match formats from 2011 onwards!
  # calculates player 1 (P1)'s probability of winning a match
  # with player 2 (P2), where P1 is the player who serves the first game of the first set
  # input:
  #   p1(p2): probability of P1(P2) winning a point in their serving game
  #   format: acceptable values are 0,1,2
  #     0 - best of 3 sets, all tiebreaks to 7
  #       for all non-grand-slams, WTA USO up to 2021
  #     1 - best of 5 sets, all tiebreaks to 7
  #       for ATP USO up to 2021
  #     2 - best of 3 sets, deciding tiebreak to 10
  #       for WTA all 2022 grand-slams + WTA AUS 2019-2021
  #     3 - best of 5 sets, deciding tiebreak to 10
  #       for ATP all 2022 grand-slams, ATP AUS 2019-2021
  #     4 - best of 3 sets, deciding set advantage set
  #       for WTA WBD up to 2018, WTA RG up to 2021, WTA AUS up to 2018
  #     5 - best of 5 sets, deciding set advantage set
  #       for ATP WBD up to 2018, ATP RG up to 2021, ATP AUS up to 2018
  #     6 - best of 3 sets, deciding set to 7 only if set is tied at 12-12
  #       for WTA WBD 2019 & 2021
  #     7 - best of 5 sets, deciding set to 7 only if set is tied at 12-12
  #       for ATP WBD 2019 & 2021
  #   p: probability model gives of P1 winning the match
  
  # !MODIFICATION! p1,p2 now should be vectors of length 19 (1-18 states + tiebreak)
  # important that the tiebreak probability is placed is the last/19th entry
  # if vector longer than 19, only first 19 elements are used
  # identically distributed assumption may be dropped now depending on p1/p2 composition
  
  # get probs of P1,P2 holding their serve
  m.p2g.p1 <- point.to.game.matrices.pw(p=p1)
  p1.g <- (solve(diag(18)-m.p2g.p1$Q)%*%m.p2g.p1$R)[1,1]
  m.p2g.p2 <- point.to.game.matrices.pw(p=p2)
  p2.g <- (solve(diag(18)-m.p2g.p2$Q)%*%m.p2g.p2$R)[1,1]
  
  # get probs of P1,P2 winning sets
  # in this block of code, probs correspond to sets where P1 serves the 1st game 
  m.g2s.p1 <- game.to.set.matrices(p=p1.g,q=p2.g)
  m.g2s.p1.w <- (solve(diag(38)-m.g2s.p1$Q)%*%m.g2s.p1$R)[1,]
  p.s1.t <- m.g2s.p1.w[1] # prob of going to tiebreak in such a set
  p1.s1.h <- m.g2s.p1.w[2] # prob of P1 winning by holding in such a set
  p1.s1.b <- m.g2s.p1.w[3] # prob of P1 winning by breaking P2's serve
  p2.s1.h <- m.g2s.p1.w[4] # prob of P2 winning by holding in such a set
  p2.s1.b <- m.g2s.p1.w[5] # prob of P2 winning by breaking P1's serve
  
  # in this block of code, probs correspond to sets where P2 serves the 1st game 
  m.g2s.p2 <- game.to.set.matrices(p=p2.g,q=p1.g)
  m.g2s.p2.w <- (solve(diag(38)-m.g2s.p2$Q)%*%m.g2s.p2$R)[1,]
  p.s2.t <- m.g2s.p2.w[1] # prob of going to tiebreak in such a set
  p2.s2.h <- m.g2s.p2.w[2] # prob of P2 winning by holding in such a set
  p2.s2.b <- m.g2s.p2.w[3] # prob of P2 winning by breaking P1's serve
  p1.s2.h <- m.g2s.p2.w[4] # prob of P1 winning by holding in such a set
  p1.s2.b <- m.g2s.p2.w[5] # prob of P1 winning by breaking P2's serve
  
  # get probs for players winning tiebreak games
  m.tb.p1 <- tb.to.7.matrices(p=p1[19],q=p2[19]) # to 7, P1 serves first
  m.tb.p1.w <- (solve(diag(48)-m.tb.p1$Q)%*%m.tb.p1$R)[1,]
  p.tb1.to7 <- m.tb.p1.w[1] # prob of the tiebreak needing more than 12 pts played
  p1.tb1.to7 <- m.tb.p1.w[2] # prob of P1 winning tiebreak when serving first
  p2.tb1.to7 <- m.tb.p1.w[3] # prob of P2 winning tiebreak when receiving first
  
  m.tb.p2 <- tb.to.7.matrices(p=p2[19],q=p1[19]) # to 7, P2 serves first
  m.tb.p2.w <- (solve(diag(48)-m.tb.p2$Q)%*%m.tb.p2$R)[1,]
  p.tb2.to7 <- m.tb.p2.w[1] # prob of the tiebreak needing more than 12 pts played
  p2.tb2.to7 <- m.tb.p2.w[2] # prob of P2 winning tiebreak when serving first
  p1.tb2.to7 <- m.tb.p2.w[3] # prob of P1 winning tiebreak when receiving first
  
  if (format==2|format==3){ # will need to 10 prob if format is 2 or 3
    m.tbd.p1 <- tb.to.10.matrices(p=p1[19],q=p2[19]) # to 10, P1 serves first
    m.tbd.p1.w <- (solve(diag(99)-m.tbd.p1$Q)%*%m.tbd.p1$R)[1,]
    p.tb1.to10 <- m.tbd.p1.w[1] # prob of the tiebreak needing more than 18 pts played
    p1.tb1.to10 <- m.tbd.p1.w[2] # prob of P1 winning tiebreak when serving first
    p2.tb1.to10 <- m.tbd.p1.w[3] # prob of P2 winning tiebreak when receiving first
    
    m.tbd.p2 <- tb.to.10.matrices(p=p2[19],q=p1[19]) # to 10, P2 serves first
    m.tbd.p2.w <- (solve(diag(99)-m.tbd.p2$Q)%*%m.tbd.p2$R)[1,]
    p.tb2.to10 <- m.tbd.p2.w[1] # prob of the tiebreak needing more than 18 pts played
    p2.tb2.to10 <- m.tbd.p2.w[2] # prob of P2 winning tiebreak when serving first
    p1.tb2.to10 <- m.tbd.p2.w[3] # prob of P1 winning tiebreak when receiving first
  }
  
  # probs for if a tiebreak needs more than 7(10) points to win
  # for non-deciding sets (to 7), chain starts at (T,A2) which is the first row
  # for deciding sets (to 10), chain starts at (T,B2) which is the second row
  # results in this block for when P1 served first in the tiebreak
  # note the player who serves first in the tiebreak served first in the set
  m.tbe.p1 <- tb.extra.matrices(p=p1[19],q=p2[19])
  m.tbe.p1.w <- solve(diag(6)-m.tbe.p1$Q)%*%m.tbe.p1$R
  p1.tbe.to7 <- m.tbe.p1.w[1,1] # P1 win prob once extra pts are played in the tiebreak
  p1.tbe.to10 <- m.tbe.p1.w[2,1] # for P2's probs, need only subtract this from 1
  
  # results in this block for when P2 served first in the tiebreak
  m.tbe.p2 <- tb.extra.matrices(p=p2[19],q=p1[19])
  m.tbe.p2.w <- solve(diag(6)-m.tbe.p2$Q)%*%m.tbe.p2$R
  p2.tbe.to7 <- m.tbe.p2.w[1,1] # P2 win prob once extra pts are played in the tiebreak
  p2.tbe.to10 <- m.tbe.p2.w[2,1] # for P1's probs, need only subtract this from 1
  
  # now that the individual pieces are established, start calculating overall set
  # winning probabilities to plug into set.to.match functions
  # since P1 was designated as serving the 1st game of the entire match
  # A(B) will be synonymous with P1(P2) in variable naming from here on in
  p.b.A <- p1.s1.b
  p.h.A <- p1.s1.h+p.s1.t*(p1.tb1.to7+p.tb1.to7*p1.tbe.to7)
  q.b.A <- p2.s1.b+p.s1.t*(p2.tb1.to7+p.tb1.to7*(1-p1.tbe.to7))
  q.h.A <- p2.s1.h
  p.b.B <- p1.s2.b+p.s2.t*(p1.tb2.to7+p.tb2.to7*(1-p2.tbe.to7))
  p.h.B <- p1.s2.h
  q.b.B <- p2.s2.b
  q.h.B <- p2.s2.h+p.s2.t*(p2.tb2.to7+p.tb2.to7*p2.tbe.to7)
  
  # sacrificing some succinctness for readability below so cases are clear
  if (format==0){
    # 0 - best of 3 sets, all tiebreaks to 7
    # for all non-grand-slams, WTA USO up to 2021
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==1) {
    # 1 - best of 5 sets, all tiebreaks to 7
    # for ATP USO up to 2021
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==2) {
    # 2 - best of 3 sets, deciding tiebreak to 10
    # for WTA all 2022 grand-slams + WTA AUS 2019-2021
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.tb1.to10+p.tb1.to10*p1.tbe.to10)
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.tb1.to10+p.tb1.to10*(1-p1.tbe.to10))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.tb2.to10+p.tb2.to10*(1-p2.tbe.to10))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.tb2.to10+p.tb2.to10*p2.tbe.to10)
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==3) {
    # 3 - best of 5 sets, deciding tiebreak to 10
    # for ATP all 2022 grand-slams, ATP AUS 2019-2021
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.tb1.to10+p.tb1.to10*p1.tbe.to10)
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.tb1.to10+p.tb1.to10*(1-p1.tbe.to10))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.tb2.to10+p.tb2.to10*(1-p2.tbe.to10))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.tb2.to10+p.tb2.to10*p2.tbe.to10)
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==4) {
    # 4 - best of 3 sets, deciding set advantage set
    # for WTA WBD up to 2018, WTA RG up to 2021, WTA AUS up to 2018
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # (this means P1 also served the first game of the set)
    m.advse.p1 <- special.set.matrices(p1.g,p2.g)
    p1.advse <- (solve(diag(3)-m.advse.p1$Q)%*%m.advse.p1$R)[1,1] 
    # take complement wrt 1 for prob of P2 winning advantage set once extra games
    # and when P1 serves the first extra game (the 13th game of the set)
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # similar to explanation above, complement wrt 1 will be used as well
    m.advse.p2 <- special.set.matrices(p2.g,p1.g)
    p2.advse <- (solve(diag(3)-m.advse.p2$Q)%*%m.advse.p2$R)[1,1]
    
    # doesn't really matter which part to add the extra game probs to
    # since they're summed in the final set-to-match function
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*p1.advse
    q.b.A.d <- p2.s1.b+p.s1.t*(1-p1.advse)
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(1-p2.advse)
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*p2.advse
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==5) {
    # 5 - best of 5 sets, deciding set advantage set
    # for ATP WBD up to 2018, ATP RG up to 2021, ATP AUS up to 2018
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # (this means P1 also served the first game of the set)
    m.advse.p1 <- special.set.matrices(p1.g,p2.g)
    p1.advse <- (solve(diag(3)-m.advse.p1$Q)%*%m.advse.p1$R)[1,1] 
    # take complement wrt 1 for prob of P2 winning advantage set once extra games
    # and when P1 serves the first extra game (the 13th game of the set)
    
    # this block for probs of P1,2 winning the advantage set
    # when extra games are played and with P1 serving the first extra game
    # similar to explanation above, complement wrt 1 will be used as well
    m.advse.p2 <- special.set.matrices(p2.g,p1.g)
    p2.advse <- (solve(diag(3)-m.advse.p2$Q)%*%m.advse.p2$R)[1,1]
    
    # doesn't really matter which part to add the extra game probs to
    # since they're summed in the final set-to-match function
    p.b.A.d <- p1.s1.b
    p.h.A.d <- p1.s1.h+p.s1.t*p1.advse
    q.b.A.d <- p2.s1.b+p.s1.t*(1-p1.advse)
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(1-p2.advse)
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*p2.advse
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==6) {
    # 6 - best of 3 sets, deciding set to 7 only if set is tied at 12-12
    # for WTA WBD 2019 & 2021
    
    # following two code blocks for probs re: deciding set goes to extra games
    # the first block's probs are for when P1 serves the first extra game
    m.wbdsp.p1 <- special.set.matrices(p1.g,p2.g)$P #wbdsp - wimbledon special
    # expm package needs to be installed for line below
    m.wbdsp.p1.w <- (m.wbdsp.p1%^%12)[1,] # 12=6+6 from 6-6 to 12-12
    p1.wbdsp.s1 <- m.wbdsp.p1.w[4] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s1 <- m.wbdsp.p1.w[5] # prob P2 wins in same scenario
    t.wbdsp.s1 <- m.wbdsp.p1.w[1] # prob goes to 12-12 and tiebreak happens
    
    # the second block's probs are for when P2 serves the first extra game
    m.wbdsp.p2 <- special.set.matrices(p2.g,p1.g)$P
    m.wbdsp.p2.w <- (m.wbdsp.p2%^%12)[1,]
    p1.wbdsp.s2 <- m.wbdsp.p2.w[5] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s2 <- m.wbdsp.p2.w[4] # prob P2 wins in same scenario
    t.wbdsp.s2 <- m.wbdsp.p2.w[1] # prob goes to 12-12 for tiebreak
    
    p.b.A.d <- p1.s1.b 
    # in 12-12 to7 tiebreak, same player serves first in the tiebreak
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.wbdsp.s1+t.wbdsp.s1*(p1.tb1.to7+p.tb1.to7*p1.tbe.to7))
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.wbdsp.s1+t.wbdsp.s1*(p2.tb1.to7+p.tb1.to7*(1-p1.tbe.to7)))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.wbdsp.s2+t.wbdsp.s2*(p1.tb2.to7+p.tb2.to7*(1-p2.tbe.to7)))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.wbdsp.s2+t.wbdsp.s2*(p2.tb2.to7+p.tb2.to7*p2.tbe.to7))
    m.s2m <- set.to.match.of3(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(7)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else if (format==7) {
    # 7 - best of 5 sets, deciding set to 7 only if set is tied at 12-12
    # for ATP WBD 2019 & 2021
    
    # following two code blocks for probs re: deciding set goes to extra games
    # the first block's probs are for when P1 serves the first extra game
    m.wbdsp.p1 <- special.set.matrices(p1.g,p2.g)$P #wbdsp - wimbledon special
    m.wbdsp.p1.w <- (m.wbdsp.p1%^%12)[1,] # expm package needs to be installed
    p1.wbdsp.s1 <- m.wbdsp.p1.w[4] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s1 <- m.wbdsp.p1.w[5] # prob P2 wins in same scenario
    t.wbdsp.s1 <- m.wbdsp.p1.w[1] # prob goes to 12-12 and tiebreak happens
    
    # the second block's probs are for when P2 serves the first extra game
    m.wbdsp.p2 <- special.set.matrices(p2.g,p1.g)$P
    m.wbdsp.p2.w <- (m.wbdsp.p2%^%12)[1,]
    p1.wbdsp.s2 <- m.wbdsp.p2.w[5] # prob P1 wins deciding set after it goes to extra games
    p2.wbdsp.s2 <- m.wbdsp.p2.w[4] # prob P2 wins in same scenario
    t.wbdsp.s2 <- m.wbdsp.p2.w[1] # prob goes to 12-12 for tiebreak
    
    p.b.A.d <- p1.s1.b 
    # in 12-12 to7 tiebreak, same player serves first in the tiebreak
    p.h.A.d <- p1.s1.h+p.s1.t*(p1.wbdsp.s1+t.wbdsp.s1*(p1.tb1.to7+p.tb1.to7*p1.tbe.to7))
    q.b.A.d <- p2.s1.b+p.s1.t*(p2.wbdsp.s1+t.wbdsp.s1*(p2.tb1.to7+p.tb1.to7*(1-p1.tbe.to7)))
    q.h.A.d <- p2.s1.h
    p.b.B.d <- p1.s2.b+p.s2.t*(p1.wbdsp.s2+t.wbdsp.s2*(p1.tb2.to7+p.tb2.to7*(1-p2.tbe.to7)))
    p.h.B.d <- p1.s2.h
    q.b.B.d <- p2.s2.b
    q.h.B.d <- p2.s2.h+p.s2.t*(p2.wbdsp.s2+t.wbdsp.s2*(p2.tb2.to7+p.tb2.to7*p2.tbe.to7))
    m.s2m <- set.to.match.of5(p.b.A,p.h.A,q.b.A,q.h.A,p.b.B,p.h.B,q.b.B,q.h.B,
                              p.b.A.d,p.h.A.d,q.b.A.d,q.h.A.d,
                              p.b.B.d,p.h.B.d,q.b.B.d,q.h.B.d)
    p <- (solve(diag(17)-m.s2m$Q)%*%m.s2m$R)[1,1]
  } else {
    p <- -1 # invalid format value
  }
  return(p)
}