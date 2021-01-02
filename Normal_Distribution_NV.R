library(stats)
library(tidyverse)


# input
p <- 12
c <- 6
g <- 1

d <- c(60, 118, 114, 79, 97, 59, 76, 108, 96, 115)


#calc statistics
calc_optimal_y_NV_normaldistr_d <- function(d,p,c,g){
  mean <- mean(d)
  sd <- sd(d)
  
  critical_ratio <- (p-c)/(p-g)
  z <- qnorm(critical_ratio)
  
  y <- mean + sd*z
  cat("Optiomal y is: ", y, "\n")
  y
}


calc_optimal_y_NV_normaldistr_d(d,p,c,g)

# calculate NV results with given y and d
# check for which y bigger profit


calc_Newsvendor_normaldistr_d <- function(y,d,p,c,g){
  mean <- mean(d)
  sd <- sd(d)
  z <- (y-mean)/sd
  z
  
  # expetcted lost sales
  ELS <- sd*(dnorm(z)-z*(1-pnorm(z)))
  cat("Expected lost sales: ", ELS, "\n")
  
  # expected sales
  ES <- mean - ELS
  cat("Expectd sales: ", ES, "\n")
  
  # Expected leftover inventory
  ELO <- y - ES
  cat("Expected leftover inventory: ", ELO, "\n")
  
  # non-stockout probability
  alpha <- pnorm(z)
  cat("non-stockout probability alpha: ", alpha, "\n")
  
  # fill rate
  beta <- 1- (ELS/mean)
  cat("Fill-rate beta: ", beta, "\n")
  
  # Expected profit
  EP <- -c*y + p *ES + g*ELO
  cat("expected profit: ", EP, "\n")
}



# check for which y bigger profit
calc_Newsvendor_normaldistr_d(94,d,p,c,g)



