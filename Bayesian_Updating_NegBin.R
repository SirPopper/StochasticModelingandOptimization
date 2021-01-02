library(stats)
library(tidyverse)
options(pillar.sigfig = 5)
#options(digits=2)

## Assignment 1 
# exe2

#input
mu <- 12
sigma <- 8 #achtung hier wurzel von varianz
SL <- 0.8
d <- c(13,12,18,12,9)



calc_matrix <- function(d, mu, sigma, critical) {
  
  #create empty df with demand
  create_df <- function(d){
    period <- seq(0, length(d))
    demand <- c(0,d)
    alpha <- rep(0, length(d)+1)
    beta <- rep(0, length(d)+1)
    mü <- rep(0, length(d)+1)
    sigma <- rep(0, length(d)+1)
    S <- rep(0, length(d)+1)
    Stockout <- rep(0, length(d)+1)
    
    df <- data.frame(period = period,
                     demand = demand,
                     alpha = alpha,
                     beta = beta,
                     mü = mü,
                     sigma = sigma,
                     S = S,
                     Stockout = Stockout)
    df <- as_tibble(df)
  }
  
  
  df <- create_df(d)
  
  #### calc alpha ####
  alpha_vector <- rep(0,length(d)+1)
  
  #calc first alpha
  alpha_vector[1] <- mu^2/sigma^2
  
  #calc following rows
  for(i in seq(2, length(d)+1)){
    j <- i - 1
    alpha_vector[i] <- alpha_vector[j] + d[j]
  }
  df$alpha <- alpha_vector
  
  
  #### calc beta ####
  beta_vector <- rep(0, length(d)+1)
  beta_vector[1] <- mu/sigma^2
  
  for(i in seq(2, length(d) +1)) {
    j <- i -1
    beta_vector[i] <- beta_vector[j] +1
  }
  df$beta <- beta_vector
  
  
  
  ### calc mü and sigma####
  df <- df %>% mutate(mü = alpha/beta, sigma = sqrt(alpha/beta^2))
  
  
  #### calc S ####
  calc_S <- function(SL, alpha, beta){
      non_stockout <- 0
      S <- 0
      while(non_stockout < SL){
        S <- S + 1
        non_stockout <- pnbinom(S, size = alpha, prob = beta/(beta+1))
      }
      S
    }
  
  # assign S values to corresponding alpha and beta
  S_vector <- rep(0, length(d)+1)
  for(i in seq(1, length(d)+1)){
    S_vector[i] <- calc_S(SL, alpha_vector[i], beta_vector[i])
  }
  df$S <- S_vector
  
  
  ### Calc stockout ####
  Stockout <- rep(0, length(d))
  
  for(i in seq(1, length(d))){
    Stockout[i+1] <- S_vector[i] - d[i]
  }
  df$Stockout <- Stockout
  df
}

#output table
calc_matrix(d, mu, sigma, SL)
