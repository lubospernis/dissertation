# Is causal match superior to standard matching procedure? 
source('functions/standardMatch.R')
source('functions/causalMatchFNN.R')


# This helper function generates y1
generate_y1 <- function(treatment_function) {
  return(y0 + eval(parse(text  = treatment_function)))
}
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

treatment_function <- '2 + 10 * x1'

## Create the synth dataset for D = 0 
# Create covariate x1 
set.seed(123) # For reproducibility
uniform <- runif(n = 400, min = 0, max = 10)
set.seed(123)
normal <- rnorm(n = 100, mean = 8)
x1 <- c(uniform, normal)
x1 <- x1[x1 <= 10 & x1 >= 0]
# Create y0 
y0 <- x1
# Define treatment
y1 <- generate_y1(treatment_function)

# Bind together to create simulation.1 d= 0 dataset
d0 <- as.data.frame(cbind(x1, y0, y1))

## Create the synth dataset for D = 1 
# Create covariate x1 
set.seed(123) # For reproducibility
uniform <- runif(n = 400, min = 0, max = 10)
set.seed(123)
normal <- rnorm(n = 100, mean = 2)
x1 <- c(uniform, normal)
x1 <- x1[x1 <= 10 & x1 >= 0]
# Create y0 
y0 <- x1
# Define treatment
y1 <- generate_y1(treatment_function)

# Bind together to create simulation.1 d= 0 dataset
d1 <- as.data.frame(cbind(x1, y0, y1))

# Clean the workspace
rm(x1, y1, y0, uniform, treatment_function, normal, generate_y1)

### Assign treatment to D = 0

set.seed(123)
random <- sample(1:nrow(d0))
treat_rows <- random[1:floor(0.5*length(random))]
d0$t <- NA
d0$t[treat_rows] <- 1
d0$t[-treat_rows] <- 0

### Create new var y (realised outcome)
d0$y <- ifelse(d0$t == 1, d0$y1, d0$y0)

# Clean the workspace
rm(random, treat_rows)

### Perform both Matchings
MSE_causal <- numeric()
MSE_standard <- numeric()
for(i in 1:100){
  # For each re-assign treatment
  d0 <- reassign_treatment(d0)
  # Define baseline cases
  true_ate <- mean(d1$y1)-mean(d1$y0)
  initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
  # Compute the mean squared error of the predictions for Standard Match with reassignment
  ms<- Match(d1, d0, 'x1')
  MSE_standard[i] <- (ms - true_ate)^2
  
  # Causal Match
  mc <- causalMatchFNN(d1, d0, 'x1')
  MSE_causal[i] <- (mc - true_ate)^2

  #
  print(paste0('Iteration: ', i))
  
}

library(ggplot2)
errors <- data.frame(
  MSE_causal,
  MSE_standard
)

ggplot(data = errors) +
  geom_boxplot(aes(x = 'Standard Match', MSE_standard)) +
  geom_boxplot(aes(x= 'Causal Match', MSE_causal)) +
  xlab('') + 
  ylab('Prediction error') +
  ggsave('images/appendix_simulation_matching_standard_error.png')
