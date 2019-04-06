# Load libraries
library(magrittr)
library(devtools)
#### START ####

## Define treatment - it will take linear form

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

# Show that we can get to the true ATE
ate_true <- numeric()
for (i in 1:1000){
  d0_shuffle <- reassign_treatment(d0)
  ate_true[i] <- mean(d0_shuffle$y[d0_shuffle$t == 1]) -mean(d0_shuffle$y[d0_shuffle$t == 0]) 
}
png('images/simulation_1_ate_true.png')
hist(ate_true)
dev.off()

rm(ate_true, d0_shuffle)

# ATE d1
mean(d1$y1) - mean(d1$y0)


##### Apply the methods #####
source('causalMatchFNN.R')

MSEs_initial <- numeric()
MSEs <- numeric()
for(i in 1:100){
  # For each re-assign treatment
  d0_reassigned <- reassign_treatment(d0)
  # Compute the mean squared error of the predictions for Causal Match with reassignment
  mse <- numeric()
  mse_initial_target <- numeric()
  for (j in 1:100){
    m<- causalMatchFNNdf(d1, d0_reassigned, 'x1')
    match_out_ate <- m$y[m$t==1] - m$y[m$t==0]
    
    true_ate <- mean(target$y1)-mean(target$y0)
    initial_ate <- mean(initial$y[initial$t == 1]) - mean(initial$y[initial$t == 0])
    mse[j] <- (match_out_ate - true_ate)^2
    mse_initial_target[j] <- (true_ate - initial_ate)^2
  }
  # Assign the mse to MSEs vector
  MSEs[i] <- mean(mse)
  MSEs_initial[i] <- mean(mse_initial_target)
  
  #
  print(paste0('Iteration: ', i))
  
}

png('images/simulation_1_error.png')
hist(MSEs)
dev.off()

## Causal Forest
replicateForests <- function(initial, target, formula) {
  initial <- reassign_treatment(initial)
  
  cf <- causalForest(formula, data=initial, treatment=initial$t, 
                     split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                     bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                     split.alpha = 0.5, cv.alpha = 0.5,
                     sample.size.total = floor(nrow(initial) / 2), sample.size.train.frac = .5,
                     mtry = ceiling(ncol(initial)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1) 
  
  predictioncf <- predict(cf, target)
  
  # Ate true
  targetATE <- mean(target$y1) - mean(target$y0)
  
  #
  InTa <- mean(initial$y[initial$t==1]) - mean(initial$y[initial$t==0])
  
  # pred error
  predE <- (mean(predictioncf) - targetATE) ^2
  # trueE 
  trueE <- (InTa - targetATE)^2
  
  return(c(predE, trueE))
  
}

forestError <- replicate(100, replicateForests(d0, d1, y ~ x1))
hist(t(forestError)[, 1], breaks = 20, main = 'causal Forest', xlab = 'Pred. Error', xlim = c(0, 200))
hist(t(forestError)[, 2], breaks = 20, col = 'red', add = T)

                      