# Load libraries
library(magrittr)
library(causalTree)
source('functions/causalMatchFNN.R')
#### START ####

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
set.seed(1234) # For reproducibility
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
ass2 <- rbind(d0[, 1:3], d1[, 1:3])
ass2$d <- c(rep(497, 0), rep(499, 1))
summary(lm(data = ass2, y1 ~ d + x1))

# Perfect fit - x1 explains all the variance
rm(ass2)


# Ass3: Overlap
# -> by definition

## Compute ATEs

# Show that we can get to the true ATE
ate_true <- numeric()
for (i in 1:1000){
  d0_shuffle <- reassign_treatment(d0)
  ate_true[i] <- mean(d0_shuffle$y[d0_shuffle$t == 1]) -mean(d0_shuffle$y[d0_shuffle$t == 0]) 
}
png('images/simulation_1_ate_true.png')
hist(ate_true, main = 'ATE from repeated treatment assignment', 
     xlab = '', 
     sub = 'The red vertical line designates the true ATE')
abline(v = mean(d0$y1) - mean(d0$y0), col = 'red')
dev.off()

rm(ate_true, d0_shuffle)

# Show that the density distributions of the covariate is different 
red <- scales::alpha('red', 0.5)
png('images/simulation_1_d0_d1.png')
plot(density(d0$x1), xlim = c(0,10), ylim = c(0, 0.15), main = '', xlab = 'x1')
polygon(density(d0$x1), col = red, border = 'white')
polygon(density(d1$x1), col = alpha('blue', 0.3), border = 'white')
legend("topright", legend = c('d1', 'd0'), 
       fill = c(alpha('blue', 0.3), red))
dev.off()
rm(red)

# Show as a consequence that d0 ate and d1 ate is different 
mean(d1$y1) - mean(d1$y0)
mean(d0$y1) - mean(d0$y0)


# After adjustment 
m <- causalMatchFNNdf_run_once(d1, d0, 'x1', 123)

red <- scales::alpha('red', 0.3)
png('images/simulation_1_d0_d1_adjusted.png')
plot(density(m$x1), xlim = c(0,10), ylim = c(0, 0.15), main = '', xlab = 'x1')
polygon(density(m$x1), col = red, border = 'white')
polygon(density(d1$x1), col = alpha('blue', 0.3), border = 'white')
legend("topright", legend = c('d1', 'matched d1'), 
       fill = c(alpha('blue', 0.3), red))
dev.off()
rm(red)

causalMatchFNN(d1, d0, 'x1', 123)
##### Apply the methods #####

NPE <- numeric()
SE <- numeric()
tauPRED <- numeric()
for(i in 1:100){
  # For each re-assign treatment
  d0 <- reassign_treatment(d0)
  # Compute the mean squared error of the predictions for Causal Match with reassignment
  match_out_ate<- causalMatchFNN(d1, d0, 'x1')
  true_ate <- mean(d1$y1)-mean(d1$y0)
  ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
  SE[i] <- (match_out_ate - true_ate)^2
  NPE[i] <- (true_ate - ate) ^2
  tauPRED[i] <- match_out_ate
  
  #
  print(paste0('Iteration: ', i))
  
}

png('images/simulation_1_error_matching_compared.png', pointsize = 16)
hist(SE, breaks = 20, main = 'causal Match',xlab = 'Error', xlim = c(0, 200))
hist(NPE, breaks = 20, col = 'red', add = T)
legend("topright", legend = c('SE', 'NPE'), 
       fill = c(1, 2),
       col = c('black', 'red'))
dev.off()

png('images/simulation_1_error_matching.png', pointsize = 16)
hist(SE, breaks = 20, main = 'causal Match', sub = round(mean(SE), 2),xlab = 'Error')
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
  initial_ate <- mean(initial$y[initial$t == 1]) - mean(initial$y[initial$t == 0])
  
  # pred error
  predE <- (mean(predictioncf) - targetATE) ^2
  # trueE 
  trueE <- (initial_ate - targetATE)^2
  
  return(c(predE, trueE, mean(predictioncf)))
  
}

forestError <- replicate(100, replicateForests(d0, d1, y ~ x1))
png('images/simulation_1_error_forest_compared.png', pointsize = 16)
hist(t(forestError)[, 1], breaks = 20, 
     main = 'causal Forest', xlab = 'Error', xlim = c(0, 200) 
     )
hist(t(forestError)[, 2], breaks = 20, col = 'red', add = T)
legend("topright", legend = c('SE', 'NPE'), 
       fill = c(1, 2),
       col = c('black', 'red'))
dev.off()

png('images/simulation_1_error_forest.png', pointsize = 16)
hist(t(forestError)[, 1], breaks = 20, main = 'causal Forest', xlab = 'Error', 
     sub = round(mean(t(forestError)[, 1]), 2))
dev.off()