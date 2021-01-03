library(MDPtoolbox)
library(dplyr)
library(tidyverse)


#infinite horizon value iteration

#initialize input info
states <- 3
decisions <- 2
prob <- 3


# c(states , probability, decisions)
P <- array(0, c(states, prob, decisions))

# decision 0: probability transition matrix
P[,,1] <- matrix(c(0.25, 0.25, 0.5,
                   0.75, 0, 0.25,
                   0.5, 0.5, 0), 
                 nrow= states, ncol = states, byrow=TRUE)

# decision 1: probability transition matrix
P[,,2] <- matrix(c(0, 0.25, 0.75,
                   0.25, 0, 0.75,
                   0.25, 0.25, 0.5), 
                 nrow = states, ncol =states, byrow=TRUE)
P

# reward matrix
# columns = number of actions
# rows = number of statse
R <- matrix(c(0.55, 0.75,
              1, 0.8,
              1.2, 1),
            nrow =states, ncol=decisions, byrow=TRUE)

#check whether costs or reward when cost R*-1
R <- -R
R

#function optimal with given epsilon
m <- mdp_value_iteration(P,R, discount = 1, epsilon = 0.0001)
m




#### function ####
#function to set values in table with given iterations
#default: discount = 0.9999999

calc_value_iteration_MDP <- function(P,R,epsilon, iterations){
  
  # initalize value_iteration_matrix
  value_iteration_matrix <- tibble(iteration = 0)
  for(i in seq(1, states)){
    value_iteration_matrix[, paste0("value_S", i)] <- 0
  }
  for(i in seq(1, states)){
    value_iteration_matrix[, paste0("converge", i)] <- 0
  }
  
  #calculate value_iteration_matrix
  for(i in seq(1, iterations)){
    
    # calc value iteration for iteration i
    m <- mdp_value_iteration(P,R, discount = 0.99999, epsilon = epsilon, max_iter = i)
    
    #add iteration
    value_iteration_matrix[i, ]$iteration <- i
    
    #add values to states
    for(j in seq(1, states)){
      value_iteration_matrix[i, j+1] <- m$V[j]
    }
    
    #calc convergence
    #first row as values after that difference to iteration - 1
    for(j in seq(1, states)){
      if(i == 1){
        value_iteration_matrix[i, j+states+1] <- m$V[j]
      } else{
        value_iteration_matrix[i, j+states+1] <- m$V[j] - V_old[j]
      }
    }
    
    # assign temporary value vector
    V_old <- m$V
    
    #calculate epsilon
    if(i ==1) {value_iteration_matrix$epsilon <- 0} #create epsilon column
    
    max_Value <- value_iteration_matrix[i , -(1:(1+states))] %>% select(-epsilon) %>% max 
    min_value <- value_iteration_matrix[i , -(1:(1+states))] %>% select(-epsilon) %>% min
    value_iteration_matrix$epsilon[i] <- max_Value-min_value
  }
  value_iteration_matrix
}


# calc value_iteration Matrix
# input epsilon and iteration
calc_value_iteration_MDP(P,R, epsilon = 0.000001, iterations = 6)



