library(MDPtoolbox)

state <- 2
decision <- 3
probabilities <- 2


# finite MDP

# c(states , probability, decisions)
P <- array(0, c(2,2,3))

# decision 0
P[,,1] <- matrix(c(0.9899, 0.0101,
                   0.8081, 0.1919), 
                  2, 2, byrow=TRUE)

# decision 1
P[,,2] <- matrix(c(0.9293, 0.0707,
                   0.7273, 0.2727), 
                 2, 2, byrow=TRUE)

# decision 2
P[,,3] <- matrix(c(0.8586, 0.1414,
                   0.5051, 0.4949), 
                 2, 2, byrow=TRUE)


# reward matrix
# columns = number of actions
# rows = number of statse
R <- matrix(c(0, 0, 0,
              8, 7, 3),
              nrow=2, ncol=3, byrow=TRUE)

P
R

#function
mdp_finite_horizon(P, R, 0.99, 2)




# finite horizon multi-prediod IM






