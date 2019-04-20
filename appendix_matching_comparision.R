# Load libraries

library(ggplot2)

# Load files
source('functions/causalMatchFNN.R')
source('functions/standardMatch.R')
d1 <- readRDS(paste0('data/d1.Rds'))
d0 <- readRDS('data/d0_unlucky.Rds')

# True ate for both locations
true_ate_d1 <- mean(d1$y1) - mean(d1$y0)
true_ate_d0 <- mean(d0$y1) - mean(d0$y0)

# Figure 1 - distribution of the x1 covariate 
d0_dis <- d0[, 1:3]
d1_dis <- d1[, 1:3]

d0_dis$d <- 0 
d1_dis$d <- 1

locCombined <- rbind(d0_dis, d1_dis)

ggplot(locCombined, aes(x1, fill = factor(d))) + 
  geom_density(alpha = 0.2)  + 
  scale_fill_discrete(name = 'Location') + ggsave('images/app_matching_comp.png')

rm(d0_dis, d1_dis, locCombined)

## Simulation - figure 2, 3, 4

# Create a helper function for re-assigning treatment
reassign_treatment <- function(dataset) {
  random <- sample(1:nrow(d0))
  treat_rows <- random[1:floor(0.5*length(random))]
  dataset$t <- NA
  dataset$t[treat_rows] <- 1
  dataset$t[-treat_rows] <- 0
  dataset$y <- ifelse(dataset$t == 1, dataset$y1, dataset$y0)
  return(dataset)
}


# Show consistency
cases <- c(10, 100, 1000)

for (j in cases) {
  diff_causal <- numeric()
  diff_standard <- numeric()
  for(i in 1:j){
    # For each re-assign treatment
    d0 <- reassign_treatment(d0)
    # Define baseline cases
    true_ate <- mean(d1$y1)-mean(d1$y0)
    initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
    # Compute the mean squared error of the predictions for Standard Match with reassignment
    ms<- Match(d1, d0, 'x1')
    diff_standard[i] <- (ms - true_ate)
    
    # Causal Match
    mc <- causalMatchFNN(d1, d0, 'x1')
    diff_causal[i] <- (mc - true_ate)
    
    #
    print(paste0('Iteration: ', i))
    
  }
  
  png(sprintf('images/app_matching_standard_sim%s.png', j))
  hist(diff_standard, breaks = 20, main = 'standard Matching', xlab = 'Difference', sub = mean(diff_standard))
  abline(v = mean(diff_standard), col = 'red')
  dev.off()
  
  png(sprintf('images/app_matching_causal_sim%s.png', j))
  hist(diff_causal, breaks = 20, main = 'causalMatch', xlab = 'Difference', sub = mean(diff_causal))
  abline(v = mean(diff_causal), col = 'red')
  dev.off()
  
  print(paste0('case ', j, ' done.'))
}

