library(stats)


#Poisson distribution
#input
lambda <- 20
alpha_crit <- 0.95
beta_crit <- 0.95



# calculate y with minimal alpha non-stockout

calc_y_with_min_alpha <- function(lambda, alpha_crit){
  alpha <- 0
  y <- 0
  while(alpha < alpha_crit){
    y <- y+1
    alpha <- ppois(y, lambda)
  }
  cat("order up to level: ", y, "\n")
  cat("alpha level: ", alpha)
}

calc_y_with_min_alpha(20,0.92)



#### calc y with given beta ####
calc_y_with_min_beta_poisson <- function(lambda, beta_crit){
    
    
  #calculating the expected loss sales with given y
  calc_ELS <- function(y){
    ELS <- 0
    for(d in seq(y,999)){
      ELS <- ELS + ((d-y) * dpois(d, lambda))
    }
    ELS
  }
  
  calc_ELS(22)
  
  #calculating the beta from ELS with given y
  calc_beta <- function(y){
    beta <- 1 - calc_ELS(y)/lambda
    beta
  }
  
  #calcuating y for crit beta
    beta <- 0
    y <- 0
    while(beta < beta_crit){
      y <- y+1
      beta <- calc_beta(y)
    }
    cat("order up to level: ", y, "\n")
    cat("beta level: ", beta)
}

calc_y_with_min_beta_poisson(20, 0.95)



# standard noromal loss function with z
standard_normal_loss_function <- function(x)  exp(-(x^2)/2)/(sqrt(2*pi)) - x*(1-pnorm(x))
standard_normal_loss_function(0.27)
