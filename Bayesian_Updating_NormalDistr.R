library(stats)
library(tidyverse)
options(pillar.sigfig = 5)

#input
sigma_input <- 30
lambda_input <- 100
tau_input <- 50
SL_input <- 0.9

d <- c(78, 61, 57, 101, 115, 72)

calc_bayesian_normal <- function(d,sigma_input, lambda_input, tau_input, SL_input){

  #create empty df
  create_df <- function(d){
    period <- seq(0, length(d))
    demand <- c(0,d)
    lambda <- rep(0, length(d)+1)
    tau_squared <- rep(0, length(d)+1)
    mü <- rep(0, length(d)+1)
    sigma_squared <- rep(0, length(d)+1)
    S <- rep(0, length(d)+1)
    Stockout <- rep(0, length(d)+1)
    
    df <- data.frame(period = period,
                     demand = demand,
                     lambda = lambda,
                     tau_squared = tau_squared,
                     mü = mü,
                     sigma_squared = sigma_squared,
                     S = S,
                     Stockout = Stockout)
    df <- as_tibble(df)
  }
  
  df <- create_df(d)
  df
  
  #### initialize ####
  df[1,] <- df[1,] %>% mutate(lambda = lambda_input, 
                    tau_squared = tau_input^2,
                    mü = lambda_input,
                    sigma_squared = tau_input^2 + sigma_input^2,
                    S = qnorm(SL_input, mü, sqrt(sigma_squared)))
  
  ### fill out table ####
  for(j in seq(1, length(d)+1)){
    j <- j +1
    for(i in seq(1, length(d))){
    df[i+1, ] <- df[i+1, ] %>% 
      mutate(lambda = (sigma_input^2*df[i,]$lambda+df[i,]$tau_squared*d[i])/df[i,]$sigma_squared,
             tau_squared = (sigma_input^2*df[i,]$tau_squared)/df[i,]$sigma_squared,
             mü = lambda,
             sigma_squared = sigma_input^2+df[i+1,]$tau_squared,
             S = qnorm(SL_input, mü, sqrt(sigma_squared)),
             Stockout = df[i,]$S - d[1]
             )
    }
  }
  df
}


#calculate matrix
df_new <- calc_bayesian_normal(d,sigma_input, lambda_input, tau_input, SL_input)
df_new


### Assignment 2 exe5
d <- rnorm(10, mean=50, sd=35)
d <- d[which(d>0)]
d <- c(28,100,7,68,12,42)

calc_bayesian_normal(d, 35, 100, 100, 0.95)


