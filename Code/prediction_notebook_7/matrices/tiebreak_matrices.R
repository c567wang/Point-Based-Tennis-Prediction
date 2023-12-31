tb.to.7.matrices <- function(p,q){
  # returns Q,R matrices for tiebreak game to 7
  # [Q | R]
  # [0 | I] is for the tiebreak game up to 6-6
  
  Q <- matrix(0,nrow=48,ncol=48)
  p.idx <- rbind(c(1,2),c(7,11),c(8,12),c(9,13),c(10,14),c(11,16),c(12,17),
                 c(13,18),c(14,19),c(15,20),c(30,35),c(31,36),c(32,37),c(33,38),
                 c(34,39),c(36,40),c(37,41),c(38,42),c(39,43))
  cp.idx <- rbind(c(1,3),c(7,12),c(8,13),c(9,14),c(10,15),c(11,17),c(12,18),
                  c(13,19),c(14,20),c(15,21),c(29,35),c(30,36),c(31,37),c(32,38),
                  c(33,39),c(35,40),c(36,41),c(37,42),c(38,43))
  q.idx <- rbind(c(2,5),c(3,6),c(4,8),c(5,9),c(6,10),c(16,23),c(17,24),
                 c(18,25),c(19,26),c(20,27),c(21,28),c(22,29),c(23,30),c(24,31),
                 c(25,32),c(26,33),c(27,34),c(40,44),c(41,45),c(42,46),c(44,47),
                 c(45,48))
  cq.idx <- rbind(c(2,4),c(3,5),c(4,7),c(5,8),c(6,9),c(16,22),c(17,23),
                  c(18,24),c(19,25),c(20,26),c(21,27),c(23,29),c(24,30),c(25,31),
                  c(26,32),c(27,33),c(28,34),c(41,44),c(42,45),c(43,46),c(45,47),
                  c(46,48))
  Q[p.idx] <- p
  Q[cp.idx] <- 1-p
  Q[q.idx] <- q
  Q[cq.idx] <- 1-q
  
  R <- matrix(0,nrow=48,ncol=3)
  R[22,2] <- 1-q
  R[28,3] <- q
  R[29,2] <- p
  R[34,3] <- 1-p
  R[35,2] <- p
  R[39,3] <- 1-p
  R[40,2] <- 1-q
  R[43,3] <- q
  R[44,2] <- 1-q
  R[46,3] <- q
  R[47,1] <- 1-p
  R[47,2] <- p
  R[48,1] <- p
  R[48,3] <- 1-p
  
  return(list("Q"=Q,"R"=R))
}

tb.extra.matrices <- function(p,q){
  # [Q | R]
  # [0 | I] is for if the game goes beyond the first 12 rallies
  # p: point win probability of the first server
  # q: point win probability of the second server
  
  Q <- matrix(0,nrow=6,ncol=6)
  Q[1,4] <- p
  Q[1,6] <- 1-p
  Q[2,3] <- 1-q
  Q[2,5] <- q
  Q[3,1] <- 1-p
  Q[4,2] <- q
  Q[5,1] <- p
  Q[6,2] <- 1-q
  
  R <- matrix(0,nrow=6,ncol=2)
  R[3,1] <- p
  R[4,1] <- 1-q
  R[5,2] <- 1-p
  R[6,2] <- q
  
  return(list("Q"=Q,"R"=R))
}

tb.to.7.eq <- function(p,q){
  # returns win probability of player A
  # who is designated as the server
  # directly using equation derived combinatorially
  # from O'Donoghue & Simmonds 2019
  # p: point win probability of the first server
  # q: point win probability of the second server
  
  eq <- p^3*(1-q)^4+4*p^4*(1-q)^3*q+3*p^3*(1-p)*(1-q)^4+6*p^5*(1-q)^2*q^2+
    16*p^4*(1-p)*(1-q)^3*q+6*p^3*(1-p)^2*(1-q)^4+4*p^5*(1-q)^2*q^3+
    30*p^4*(1-p)*(1-q)^3*q^2+40*p^3*(1-p)^2*(1-q)^4*q+10*p^2*(1-p)^3*(1-q)^5+
    5*p^5*(1-q)^2*q^4+50*p^4*(1-p)*(1-q)^3*q^3+100*p^3*(1-p)^2*(1-q)^4*q^2+
    50*p^2*(1-p)^3*(1-q)^5*q+5*p*(1-p)^4*(1-q)^6+6*p^6*(1-q)*q^5+
    75*p^5*(1-p)*(1-q)^2*q^4+200*p^4*(1-p)^2*(1-q)^3*q^3+
    150*p^3*(1-p)^3*(1-q)^4*q^2+30*p^2*(1-p)^4*(1-q)^5*q+p*(1-p)^5*(1-q)^6+
    p*(1-q)*(p^6*q^6+36*p^5*(1-p)*(1-q)*q^5+225*p^4*(1-p)^2*(1-q)^2*q^4+
               400*p^3*(1-p)^3*(1-q)^3*q^3+225*p^2*(1-p)^4*(1-q)^4*q^2+
               36*p*(1-p)^5*(1-q)^5*q+(1-p)^6*(1-q)^6)/(1-p*q-(1-p)*(1-q))
  return(eq)
}

tb.to.10.matrices <- function(p,q){
  # returns Q1,R1,Q2,R2 matrices for tiebreak game to 7
  # [Q1 | R1]
  # [0  | I ] is for the tiebreak game up to 6-6
  # [Q2 | R2]
  # [0  | I ] is for if the game goes beyond the first 12 rallies
  # p: point win probability of the first server
  # q: point win probability of the second server
  
  Q <- matrix(0,nrow=99,ncol=99)
  p.idx <- rbind(c(1,2),c(7,11),c(8,12),c(9,13),c(10,14),c(11,16),c(12,17),
                 c(13,18),c(14,19),c(15,20),c(29,37),c(30,38),c(31,39),c(32,40),
                 c(33,41),c(34,42),c(35,43),c(36,44),c(37,46),c(38,47),c(39,48),
                 c(40,49),c(41,50),c(42,51),c(43,52),c(44,53),c(45,54),c(66,73),
                 c(67,74),c(68,75),c(69,76),c(70,77),c(71,78),c(72,79),c(74,80),
                 c(75,81),c(76,82),c(77,83),c(78,84),c(79,85),c(92,95),c(93,96),
                 c(94,97),c(96,98),c(97,99))
  # cp.idx <- p.idx + c(0,1) does not work even for first half entries, as the
  # addition works by treating p.idx as a multi-dim array going by column
  cp.idx <- rbind(c(1,3),c(7,12),c(8,13),c(9,14),c(10,15),c(11,17),c(12,18),c(13,19),
                  c(14,20),c(15,21),c(29,38),c(30,39),c(31,40),c(32,41),c(33,42),c(34,43),
                  c(35,44),c(36,45),c(37,47),c(38,48),c(39,49),c(40,50),c(41,51),c(42,52),
                  c(43,53),c(44,54),c(45,55),c(65,73),c(66,74),c(67,75),c(68,76),c(69,77),
                  c(70,78),c(71,79),c(73,80),c(74,81),c(75,82),c(76,83),c(77,84),c(78,85),
                  c(91,95),c(92,96),c(93,97),c(95,98),c(96,99))
  q.idx <- rbind(c(2,5),c(3,6),c(4,8),c(5,9),c(6,10),c(16,23),c(17,24),c(18,25),c(19,26),
                 c(20,27),c(21,28),c(22,30),c(23,31),c(24,32),c(25,33),c(26,34),c(27,35),
                 c(28,36),c(46,56),c(47,57),c(48,58),c(49,59),c(50,60),c(51,61),c(52,62),
                 c(53,63),c(54,64),c(56,65),c(57,66),c(58,67),c(59,68),c(60,69),c(61,70),
                 c(62,71),c(63,72),c(80,86),c(81,87),c(82,88),c(83,89),c(84,90),c(86,91),
                 c(87,92),c(88,93),c(89,94))
  cq.idx <- rbind(c(2,4),c(3,5),c(4,7),c(5,8),c(6,9),c(16,22),c(17,23),c(18,24),c(19,25),
                  c(20,26),c(21,27),c(22,29),c(23,30),c(24,31),c(25,32),c(26,33),c(27,34),
                  c(28,35),c(47,56),c(48,57),c(49,58),c(50,59),c(51,60),c(52,61),c(53,62),
                  c(54,63),c(55,64),c(57,65),c(58,66),c(59,67),c(60,68),c(61,69),c(62,70),
                  c(63,71),c(64,72),c(81,86),c(82,87),c(83,88),c(84,89),c(85,90),c(87,91),
                  c(88,92),c(89,93),c(90,94))
  Q[p.idx] <- p
  Q[cp.idx] <- 1-p
  Q[q.idx] <- q
  Q[cq.idx] <- 1-q
  
  R <- matrix(0,nrow=99,ncol=3)
  R[46,2] <- 1-q
  R[55,3] <- q
  R[56,2] <- 1-q
  R[64,3] <- q
  R[65,2] <- p
  R[72,3] <- 1-p
  R[73,2] <- p
  R[79,3] <- 1-p
  R[80,2] <- 1-q
  R[85,3] <- q
  R[86,2] <- 1-q
  R[90,3] <- q
  R[91,2] <- p
  R[94,3] <- 1-p
  R[95,2] <- p
  R[97,3] <- 1-p
  R[98,1] <- q
  R[98,2] <- 1-q
  R[99,1] <- 1-q
  R[99,3] <- q
  
  return(list("Q"=Q,"R"=R))
}