# assignment 1


# Exercise 1 b)
library(stats)

lambda <- 20
beta_crit <- 0.95

#calculating the expected loss sales
ELS_calc <- function(y){
  ELS <- 0
  for(d in seq(y+1,60)){
    ELS <- ELS + ((d-y) * dpois(d, lambda))
  }
  ELS
}

ELS_calc(22)

#calculating the beta from ELS
beta_calc <- function(x){
  beta <- 1 - ELS_calc(x)/lambda
  beta
}

#Variant 1: with a sequence
#sequence of potential y*
y_seq <- seq(20,30)

#apply sequence to beta function
sapply(y_seq, beta_calc)

y_seq[min(which(sapply(y_seq, beta_calc)>=0.95))]


#Variant 2: with a function
y_calc <- function(y){
      beta <- 0
      while(beta < beta_crit){
        y <- y+1
        beta <- beta_calc(y)
      }
  cat("order up to level: ", y, "\n")
  cat("beta level: ", beta)
  y
}

solution <- y_calc(10)
solution




