# The aim of this simulation is to show that knn breaks down as the num of covariates increases
# p = 6, p = 20
source('functions/causalMatchFNN.R')

### First create data 
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


# Create a helper function which adds a random covariate
add_rand <- function(df, name, seed = NULL){
  set.seed(seed)
  rand <- rnorm(n = nrow(df), mean = 10, sd = 10)
  df[, name] <- rand
  return(df)
}

# Situation 1; p = 6
covariates <- paste0('x', 2:6)
true_ate <- mean(d1$y1) - mean(d1$y0)
MSE <- numeric()

MSE <- (causalMatchFNN(d1, d0, 'x1') - true_ate) ^ 2

for (i in covariates) {
  # New random covariates
  d1 <- add_rand(d1, i)
  d0 <- add_rand(d0, i)
  
  prediction <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
  local_mse <- (prediction - true_ate)^2
  MSE <- c(MSE, local_mse)
  
  print(paste0('Adding covariate: ', i))
}

plot(MSE, type = 'o')

# Clean
d0 <- d0[, 1:5]
d1 <- d1[, 1:3]

# Situation 2; p = 20
# Situation 1; p = 6
covariates <- paste0('x', 2:20)
true_ate <- mean(d1$y1) - mean(d1$y0)
MSE <- numeric()

MSE <- (causalMatchFNN(d1, d0, 'x1') - true_ate) ^ 2

for (i in covariates) {
  # New random covariates
  d1 <- add_rand(d1, i)
  d0 <- add_rand(d0, i)
  
  prediction <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
  local_mse <- (prediction - true_ate)^2
  MSE <- c(MSE, local_mse)
  
  print(paste0('Adding covariate: ', i))
}

plot(MSE, type = 'o')

library(causalTree)

formula <- paste0(grep('x', colnames(d0), value = T), collapse= '+')
formula <- paste0('y~',formula)
formula <- as.formula(formula)

causacf <- causalForest(y~x1 +x2 , data=d0, treatment=d0$t, 
                              split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                              bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                              split.alpha = 0.5, cv.alpha = 0.5,
                              sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                              mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=2,ncov_sample=2) 

(mean(predict(causacf, d1)) - true_ate) ^ 2
library(corrplot)
cor(d0[, c(1, 6:24)])

corrplot(cor(d0[, c(1, 5:24)]))
