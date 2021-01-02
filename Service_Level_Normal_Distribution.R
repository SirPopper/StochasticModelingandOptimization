library(stats)


#input
mü <- 20
sigma <- sqrt(20)
alpha_crit <- 0.92


#### calc y with given alpha for normal distr####

calc_y_with_min_alpha_normal <- function(alpha_crit, mü, sigma){
    alpha <- 0
    y <- 0
    while(alpha < alpha_crit){
      y <- y +1
      alpha <- pnorm(y, mü, sigma)
    }
    cat("optimal y: ", y, "\n")
    cat("alpha level: ", alpha, "\n")
}


calc_y_with_min_alpha_normal(0.92, mü, sigma)
