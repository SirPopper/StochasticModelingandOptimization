library(stats)
library(tidyverse)


# input
p <- 12
c <- 6
g <- 1

d <- c(60, 118, 114, 79, 97, 59, 76, 108, 96, 115)



calc_NV_distribution_free <- function(d,y,p,c,g) {
  mean <- mean(d)
  sd <- sd(d)
  
  # calc y* with scarf's rule
  if(y==0){
  y <- mean + (sd/2)*(sqrt((p-c)/(c-g))-sqrt((c-g)/(p-c)))
  cat("Optimal y Scarf's rule: ", y, "\n")
  }
  
  z <- (y-mean)/sd
  cat("z: ", y, "\n")
  # expected lost sales upper bound
  ELS <- (sqrt(sd^2+(y-mean)^2) -(y-mean))/2
  cat("Exptected lost sales: ", ELS, "\n")
  
  # Expected sales
  ES <- mean-ELS
  cat("Exptected sales: ", ES, "\n")
  
  # Expected leftover inventory
  ELO <- y - ES
  cat("Exptected leftover inventory: ", ELO, "\n")
  
  # Expected profit upper bound (cauchy schwarz)
  EP_UB <- (p-g)*mean-(c-g)*y-(p-g)*ELS
  cat("Exptected Profit Upper Bound (Cauchy Schwarz): ", EP_UB, "\n")
 
  #lower bound scarf rule
  m <- p/c-1
  d <- 1-g/c
  EP_LB <- c*m*mean*(1-sd/mean*sqrt(d/m))
  cat("Exptected Profit Lower Bound (Scarf's RUle): ", EP_LB, "\n")
}

#calculate y = 0 for optimal y
calc_NV_distribution_free(d=d,y=94,p=p,c=c,g=g)



