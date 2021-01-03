library(tidyverse)
options(pillar.sigfig = 2)
options(digits=2)

#input

states <- 2
decisions <- 3

alpha <- 0.99
prob <- c(0.9899, 0.9293, 0.8586, 0.8081, 0.7273, 0.5051)

direct_cost <- c(0, 0.5,0.5)
revenue <- c(8,7,3)

t <- 1


#create empty data frame
create_df <- function(states, decisions){
  df <- data.frame(state = rep(0, states*decisions),
           decision = rep(0, states*decisions))
  df <- as_tibble(df)
  
  # generate matrix
  seq_1 <- c()
  for(i in seq(1,states)){
    seq_1 <- c(seq_1, rep(i, decisions))
  }
  df$state <- seq_1
  
  # create states
  seq_2 <- c()
  for(i in seq(1, states)){
    seq_2 <- c(seq_2, seq(1, decisions))
  }
  seq_2
  df$decision <- seq_2
  
  
  df
}

df <- create_df(states, decisions)
df

# create probabilities
df <- df %>% mutate(prob_yes = 1-prob, prob_no = prob)
df


# create costs
df <- df %>% mutate(cost = 0)
for(i in seq(1,decisions*states)){
  df[i, ]$cost <- direct_cost[df[i, ]$decision]
}

# create revenue
df <- df %>% mutate(no_purchase = 0)
df <- df %>% mutate(purchase = 0)
for(i in seq(1,decisions*states)){
  df[i, ]$purchase <- revenue[df[i, ]$decision]
}


# calc first reward
reward <- NULL
for(i in seq(1,decisions*states)){
  reward <- c(reward,alpha*(df[i,]$prob_no*df[i,]$no_purchase+df[i,]$prob_yes*df[i,]$purchase)-df[i,]$cost)
}
df$reward <- reward
df

# add decisions to state_decision Matrix
state_decision <- data.frame(t = seq(1,t))
for(i in seq(1, states)) {                                       
  state_decision[ , ncol(state_decision) + 1] <- 0                  
  colnames(state_decision)[ncol(state_decision)] <- paste0("state", i)
}

for(i in seq(1,states)){
  state_decision[t,i+1] <- df %>% filter(state == i) %>% top_n(n = 1) %>% pull(decision)
}

state_decision

# safe max reward from first iteration
temp_reward <- NULL
for(i in seq(1, states)){
  temp_reward <- c(temp_reward,df %>% filter(state == i) %>% top_n(n = 1) %>% pull(reward))
}
temp_reward




