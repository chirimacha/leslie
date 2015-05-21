
 
  leslie<-function(f12,f13,f14,f15,f16,t12,t23,t34,t45,t56,t66) {
    A0 <- matrix(0, nrow=6, ncol=6)
    A0[1, 2] <- f12 # fecundity, class 2
    A0[1, 3] <-  f13# fecundity, class 3
    A0[1, 4] <- f14  # fecundity, class 4
     A0[1, 5] <- f15  # fecundity, class 5
     A0[1, 6] <- f16  # fecundity, adult
    A0[2, 1] <- t12 # transition probability, class 1 to 2
    A0[3, 2] <- t23 # transition probability, class 2 to 3
    A0[4, 3] <- t34 # transition probability, class 3 to 4
    A0[5, 4] <- t45 # transition probability, class 4 to 5
    A0[6, 5] <- t56 # transition probability, class 5 to adult
    A0[6, 6] <- t66 # survival probability, adult
    lambda <- eigen(A0)$values[1] # dominant eigenvalue
    rvec <- eigen(A0)$vectors[, 1] # right eigenvector
    lvec <- eigen(t(A0))$vectors[, 1] # left eigenvector

    
#   out<-list(A0=A0, emat=emat, x=x, Nt=Nt, px=px, Lambda=lambda)
   out<-list(A0=A0, Lambda=lambda, stablestagedistribution=rvec)
   return(out)
  }

#example run function
#fecundity only occurs in adult stage, 20 offspring
leslie(0,0,0,0,20,.7,.7,.8,.8,.8,.9)





