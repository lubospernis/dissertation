# The aim of this simulation is to show that knn breaks down as the num of covariates increases
# p = 6, p = 20
source('functions/causalMatchFNN.R')
library(causalTree)

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
initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
# Create an empty vector to capture MSE from Matching
MSE_matching <- numeric()
# Create an empty vector to capture MSE from Forests
MSE_forest <- numeric()
# Matching
MSE_matching <- (causalMatchFNN(d1, d0, 'x1') - true_ate) ^ 2
# Forest
cf <- causalForest(y~x1 , data=d0, treatment=d0$t, 
                   split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                   bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                   split.alpha = 0.5, cv.alpha = 0.5,
                   sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                   mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1)
MSE_forest <- (mean(predict(cf, d1)) - true_ate) ^ 2

for (i in covariates) {
  # New random covariates
  d1 <- add_rand(d1, i)
  d0 <- add_rand(d0, i)
  # Matching
  prediction_matching <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
  local_matching_mse <- (prediction_matching - true_ate)^2
  MSE_matching <- c(MSE_matching, local_matching_mse)
  
  # Forest
  formula <- paste0(grep('x', colnames(d0), value = T), collapse= '+')
  formula <- paste0('y~',formula)
  formula <- as.formula(formula)
  
  cf <- causalForest(formula,
                     data=d0,
                     treatment=d0$t, 
                     split.Rule="CT",
                     split.Honest=T,
                     split.Bucket=F,
                     bucketNum = 5,
                     bucketMax = 100,
                     cv.option="CT",
                     cv.Honest=T,
                     minsize = 2L, 
                     split.alpha = 0.5,
                     cv.alpha = 0.5,
                     sample.size.total = floor(nrow(d0) / 2),
                     sample.size.train.frac = .5,
                     mtry = ceiling(ncol(d0)/3),
                     nodesize = 3,
                     num.trees= 5,
                     ncolx=length(grep('x', colnames(d0))),
                     ncov_sample= length(grep('x', colnames(d0)))
                     )
  
  MSE_forest <- c(MSE_forest, (mean(predict(cf, d1)) - true_ate) ^ 2)
  
  print(paste0('Adding covariate: ', i))
}

png('images/simulation_2_p6.png')
plot(MSE_matching, type = 'o', ylim = c(0, 220))
lines(MSE_forest, col ='blue', type = 'o')
abline(h = (initial_ate - true_ate) ^ 2)
dev.off()

# Clean
d0 <- d0[, 1:5]
d1 <- d1[, 1:3]

# Situation 2; p = 20
covariates <- paste0('x', 2:20)
true_ate <- mean(d1$y1) - mean(d1$y0)
initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
# Create an empty vector to capture MSE from Matching
MSE_matching <- numeric()
# Create an empty vector to capture MSE from Forests
MSE_forest <- numeric()
# Matching
MSE_matching <- (causalMatchFNN(d1, d0, 'x1') - true_ate) ^ 2
# Forest
cf <- causalForest(y~x1 , data=d0, treatment=d0$t,
                   split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                   bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                   split.alpha = 0.5, cv.alpha = 0.5,
                   sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                   mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1)
MSE_forest <- (mean(predict(cf, d1)) - true_ate) ^ 2

for (i in covariates) {
  # New random covariates
  d1 <- add_rand(d1, i)
  d0 <- add_rand(d0, i)
  # Matching
  prediction_matching <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
  local_matching_mse <- (prediction_matching - true_ate)^2
  MSE_matching <- c(MSE_matching, local_matching_mse)

  # Forest
  formula <- paste0(grep('x', colnames(d0), value = T), collapse= '+')
  formula <- paste0('y~',formula)
  formula <- as.formula(formula)

  cf <- causalForest(formula,
                     data=d0,
                     treatment=d0$t,
                     split.Rule="CT",
                     split.Honest=T,
                     split.Bucket=F,
                     bucketNum = 5,
                     bucketMax = 100,
                     cv.option="CT",
                     cv.Honest=T,
                     minsize = 2L,
                     split.alpha = 0.5,
                     cv.alpha = 0.5,
                     sample.size.total = floor(nrow(d0) / 2),
                     sample.size.train.frac = .5,
                     mtry = ceiling(ncol(d0)/3),
                     nodesize = 3,
                     num.trees= 5,
                     ncolx=length(grep('x', colnames(d0))),
                     ncov_sample= length(grep('x', colnames(d0)))
  )

  MSE_forest <- c(MSE_forest, (mean(predict(cf, d1)) - true_ate) ^ 2)

  print(paste0('Adding covariate: ', i))
}

png('images/simulation_2_p20.png')
plot(MSE_matching, type = 'o', ylim = c(0, 400))
lines(MSE_forest, col ='blue', type = 'o')
abline(h = (initial_ate - true_ate) ^ 2)
dev.off()

###### #######
MSE_forest_repeated <- data.frame(
  line = rep(1, 6), 
  p = seq(from = 1, to = 6, by = 1), 
  MSE = MSE_forest[1:6]
)
MSE_matching_repeated <- data.frame(
  line = rep(1, 6), 
  p = seq(from = 1, to = 6, by = 1), 
  MSE = MSE_matching[1:6]
)


# Define the loop as function to simulate the process
loop_random_variables <- function() {
  covariates <- paste0('x', 2:6)
  true_ate <- mean(d1$y1) - mean(d1$y0)
  initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
  # Create an empty vector to capture MSE from Matching
  MSE_matching <- numeric()
  # Create an empty vector to capture MSE from Forests
  MSE_forest <- numeric()
  # Matching
  MSE_matching <- (causalMatchFNN(d1, d0, 'x1') - true_ate) ^ 2
  # Forest
  cf <- causalForest(y~x1 , data=d0, treatment=d0$t,
                     split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                     bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                     split.alpha = 0.5, cv.alpha = 0.5,
                     sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                     mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1)
  MSE_forest <- (mean(predict(cf, d1)) - true_ate) ^ 2

  for (i in covariates) {
    # New random covariates
    d1 <- add_rand(d1, i)
    d0 <- add_rand(d0, i)
    # Matching
    prediction_matching <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
    local_matching_mse <- (prediction_matching - true_ate)^2
    MSE_matching <- c(MSE_matching, local_matching_mse)

    # Forest
    formula <- paste0(grep('x', colnames(d0), value = T), collapse= '+')
    formula <- paste0('y~',formula)
    formula <- as.formula(formula)

    cf <- causalForest(formula,
                       data=d0,
                       treatment=d0$t,
                       split.Rule="CT",
                       split.Honest=T,
                       split.Bucket=F,
                       bucketNum = 5,
                       bucketMax = 100,
                       cv.option="CT",
                       cv.Honest=T,
                       minsize = 2L,
                       split.alpha = 0.5,
                       cv.alpha = 0.5,
                       sample.size.total = floor(nrow(d0) / 2),
                       sample.size.train.frac = .5,
                       mtry = ceiling(ncol(d0)/3),
                       nodesize = 3,
                       num.trees= 5,
                       ncolx=length(grep('x', colnames(d0))),
                       ncov_sample= length(grep('x', colnames(d0)))
    )

    MSE_forest <- c(MSE_forest, (mean(predict(cf, d1)) - true_ate) ^ 2)

    print(paste0('Adding covariate: ', i))
  }

  return(list(matching = MSE_matching, 
              forest = MSE_forest))
}


# Now add new lines
for (i in 2:100) {
  additional_lines <- loop_random_variables()
  MSE_matching_repeated <- rbind(MSE_matching_repeated, 
                                 data.frame(
                                   line = rep(i, 6), p = 1:6, MSE = additional_lines$matching
                                   )
                                 )
  MSE_forest_repeated <- rbind(MSE_forest_repeated, 
                                 data.frame(
                                   line = rep(i, 6), p = 1:6, MSE = additional_lines$forest
                                 )
  )
  
}

AggForest <- aggregate(MSE_forest_repeated, list(MSE_forest_repeated$p), mean)
AggMatching <- aggregate(MSE_matching_repeated, list(MSE_matching_repeated$p), mean)


# Save
count <- 7
png('images/simulation_2_p6_repeated.png', width = 600, height = 600, pointsize = 15)
plot(MSE_matching_repeated[1:6, 'MSE'], col = 'pink', ylim = c(0, 400), type = 'l', 
     xlab = 'Number of covariates', ylab = 'MSE')
lines(MSE_forest_repeated[1:6, 'MSE'], col ='lightblue')
abline(h = (initial_ate - true_ate) ^ 2)
while(count < (nrow(MSE_matching_repeated) - 5)) {
  lines(MSE_forest_repeated[count:(count+5), 'MSE'], col ='lightblue')
  lines(MSE_matching_repeated[count:(count+5), 'MSE'], col ='pink')
  count <- count + 6
}
lines(AggForest$MSE, col = 'blue')
lines(AggMatching$MSE, col = 'red')
legend("topright", legend = c('Causal Forest', 'Causal Match'), 
       lty = c(1, 1),
       col = c('blue', 'red'))
dev.off()

