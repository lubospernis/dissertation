# Load libraries
library(magrittr)
library(FNN)
#### START ####

## Define treatment - it will take linear form

# This helper function generates y1
generate_y1 <- function(treatment_function) {
  return(y0 + eval(parse(text  = treatment_function)))
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

### Checking assumptions

# Ass1: Random asisgnment
cor(d0[, c('t', 'x1', 'y0', 'y1')])

# Ass2: 

# Ass3: Overlap
# -> by definition

## Compute ATEs

# ATE d0
mean(d0$y1) - mean(d0$y0)
mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])

# ATE d1
mean(d1$y1) - mean(d1$y0)


##### Apply the methods #####
source('causalMatchFNN.R')

# Create a helper function for re-assigning treatment
reassign_treatment <- function(dataset) {
  random <- sample(1:nrow(d0))
  treat_rows <- random[1:floor(0.5*length(random))]
  dataset$t <- NA
  dataset$t[treat_rows] <- 1
  dataset$t[-treat_rows] <- 0
  return(dataset)
}

MSEs <- numeric()
for(i in 1:1000){
  # For each re-assign treatment
  d0_reassigned <- reassign_treatment(d0)
  # Compute the mean squared error of the predictions for Causal Match with reassignment
  match_out <- replicate(100, causalMatchFNN(d1, d0, 'x1'))
  match_out_matrix <- matrix(unlist(match_out), ncol = 3)
  pred_errors <- (match_out_matrix[, 3] - match_out_matrix[, 2])^2
  mse <- mean(pred_errors)
  # Assign the mse to MSEs vector
  MSEs[i] <- mse
  #
  print(paste0('Iteration: ', i))
  
}

png('images/simulation_1_error.png')
hist(MSEs)
dev.off()

