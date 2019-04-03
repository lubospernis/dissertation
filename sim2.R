library(causalTree)

data('simulation.1')

# Some generic functions
split_locations <- function(dataset, seed = NULL){
  set.seed(seed)
  rand <- sample(1:(nrow(dataset)))
  dataset$rand <- rand
  
  dataset<- dataset[order(dataset$rand), ]
  
  
  d0 <- dataset[1:(length(rand) / 2), ]
  d1 <- dataset[((length(rand) / 2) + 1):length(rand),]
  d0$rand <- NULL
  d1$rand <- NULL
  
  dfs <- list(d0 = d0, d1 = d1)
  
  return(dfs)
}
#case 1 - I can do this fun straight away because random

random_location <- function(dataset) {
  rand <- sample(1:(nrow(dataset)))
  dataset$rand <- rand
  
  dataset<- dataset[order(dataset$rand), ]
  
  
  d0 <- dataset[1:(length(rand) / 2), ]
  d1 <- dataset[((length(rand) / 2) + 1):length(rand),]
  
  # ate
  loc1 <- mean(d0$y[d0$treatment == 1]) - mean(d0$y[d0$treatment == 0]) 
  
  loc2 <- mean(d1$y[d1$treatment == 1]) - mean(d1$y[d1$treatment == 0])
  
  return(loc1 - loc2)
  
}

# plot and see that it is unbiased - this is if the location was independent of X
plot(density(replicate(1000, random_location(simulation.1))))


# case 2 - constant treatment effects
# Assume that tau is the same for every i

# So how do we do?
constant_effect <- function(dataset, covariate, treatment, effect){
  dataset$y_new <- ifelse(treatment == 0, covariate, covariate + effect)
  # Now split into two locations
  rand <- sample(1:(nrow(dataset)))
  dataset$rand <- rand
  
  dataset<- dataset[order(dataset$rand), ]
  
  
  d0 <- dataset[1:(length(rand) / 2), ]
  d1 <- dataset[((length(rand) / 2) + 1):length(rand),]
  
  # Return the difference between locations
  loc1 <- mean(d0$y_new[d0$treatment == 1]) - mean(d0$y_new[d0$treatment == 0]) 
  loc2 <- mean(d1$y_new[d1$treatment == 1]) - mean(d1$y_new[d1$treatment == 0])
  # Return the difference
  print(paste0("ATE0: ", loc1))
  print(paste0("ATE1: ", loc1))
  
  
  return(loc1 - loc2)
  
}

replicate(1000, constant_effect(simulation.1, simulation.1$x1, simulation.1$treatment, 10)) %>%
  density(.) %>% plot(.)

# Case 3 - Linear effects
 
# First create sick and healthy pop

# SICK
sample(which(simulation.1$x1 < 0) ,100)
