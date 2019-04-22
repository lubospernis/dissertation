# The aim of this simulation is to show that knn breaks down as the num of covariates increases
# p = 6, p = 20; p = 80
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
set.seed(1234) # For reproducibility
uniform <- runif(n = 400, min = 0, max = 10)
set.seed(1234)
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
  #rand <- runif(n = nrow(df), min = 0, max = 10)s
  rand <- runif(n = nrow(df), min = 1, max = 10)
  df[, name] <- rand
  return(df)
}

# Situation 1; p = 6
covariates <- paste0('x', 2:6)
target_true_ate <- mean(d1$y1) - mean(d1$y0)
initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
# Create an empty vector to capture MSE from Matching
SE_matching <- numeric()
# Create an empty vector to capture MSE from Forests
SE_forest <- numeric()
# Matching
SE_matching <- (causalMatchFNN(d1, d0, 'x1') - target_true_ate) ^ 2
# Forest
cf <- causalForest(y~x1 , data=d0, treatment=d0$t, 
                   split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                   bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                   split.alpha = 0.5, cv.alpha = 0.5,
                   sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                   mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1)
SE_forest <- (mean(predict(cf, d1)) - target_true_ate) ^ 2

for (i in covariates) {
  # New random covariates
  d1 <- add_rand(d1, i)
  d0 <- add_rand(d0, i)
  # Matching
  tauPRED_match <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
  SE_matching_local <- (tauPRED_match - target_true_ate)^2
  SE_matching <- c(SE_matching, SE_matching_local)
  
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
  
  SE_forest <- c(SE_forest, (mean(predict(cf, d1)) - target_true_ate) ^ 2)
  
  print(paste0('Adding covariate: ', i))
}

png('images/simulation_2_p6.png')
plot(SE_matching, type = 'o', ylim = c(0, max(SE_forest)), ylab = 'Squared Error', col = 'red')
lines(SE_forest, col ='blue', type = 'o')
legend("topright", legend = c('Causal Forest', 'Causal Match'), 
       lty = c(1, 1),
       col = c('blue', 'red'))
dev.off()

# Clean
d0 <- d0[, 1:5]
d1 <- d1[, 1:3]

#Situation 1; p = 80
covariates <- paste0('x', 2:80)
target_true_ate <- mean(d1$y1) - mean(d1$y0)
initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
# Create an empty vector to capture MSE from Matching
SE_matching <- numeric()
# Create an empty vector to capture MSE from Forests
SE_forest <- numeric()
# Matching
SE_matching <- (causalMatchFNN(d1, d0, 'x1') - target_true_ate) ^ 2
# Forest
cf <- causalForest(y~x1 , data=d0, treatment=d0$t,
                   split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                   bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                   split.alpha = 0.5, cv.alpha = 0.5,
                   sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                   mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1)
SE_forest <- (mean(predict(cf, d1)) - target_true_ate) ^ 2

for (i in covariates) {
  # New random covariates
  d1 <- add_rand(d1, i)
  d0 <- add_rand(d0, i)
  # Matching
  tauPRED_match <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
  SE_matching_local <- (tauPRED_match - target_true_ate)^2
  SE_matching <- c(SE_matching, SE_matching_local)

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

  SE_forest <- c(SE_forest, (mean(predict(cf, d1)) - target_true_ate) ^ 2)

  print(paste0('Adding covariate: ', i))
}

png('images/simulation_2_p80.png')
plot(SE_matching, type = 'o', ylim = c(0, 230), ylab = 'Squared Error', col = 'red')
lines(SE_forest, col ='blue', type = 'o')
abline(h = (initial_ate - target_true_ate) ^ 2)
legend("topright", legend = c('Causal Forest', 'Causal Match'),
       lty = c(1, 1),
       col = c('blue', 'red'))
dev.off()

###### #######
# Repeated; p = 20

SE_forest_repeated <- data.frame(
  line = rep(1, 20), 
  p = seq(from = 1, to = 20, by = 1), 
  SE = SE_forest[1:20]
)
SE_matching_repeated <- data.frame(
  line = rep(1, 20), 
  p = seq(from = 1, to = 20, by = 1), 
  SE = SE_matching[1:20]
)


# Define the loop as function to simulate the process
loop_random_variables <- function() {
  d0 <- d0[, 1:5]
  d1 <- d1[, 1:3]
  
  
  covariates <- paste0('x', 2:20)
  target_true_ate <- mean(d1$y1) - mean(d1$y0)
  initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
  # Create an empty vector to capture SE from Matching
  SE_matching <- numeric()
  # Create an empty vector to capture SE from Forests
  SE_forest <- numeric()
  # Matching
  SE_matching <- (causalMatchFNN(d1, d0, 'x1') - target_true_ate) ^ 2
  # Forest
  cf <- causalForest(y~x1 , data=d0, treatment=d0$t,
                     split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                     bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                     split.alpha = 0.5, cv.alpha = 0.5,
                     sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                     mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1)
  SE_forest <- (mean(predict(cf, d1)) - target_true_ate) ^ 2
  
  for (i in covariates) {
    # New random covariates
    d1 <- add_rand(d1, i)
    d0 <- add_rand(d0, i)
    # Matching
    tauPRED_match <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
    SE_matching_local <- (tauPRED_match - target_true_ate)^2
    SE_matching <- c(SE_matching, SE_matching_local)
    
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
    
    SE_forest <- c(SE_forest, (mean(predict(cf, d1)) - target_true_ate) ^ 2)
    
    print(paste0('Adding covariate: ', i))
    
  }
  
  
  return(list(matching = SE_matching, 
              forest = SE_forest))
}


# Now add new lines
for (i in 2:10) {
  additional_lines <- loop_random_variables()
  SE_matching_repeated <- rbind(SE_matching_repeated, 
                                data.frame(
                                  line = rep(i, 20), p = 1:20, SE = additional_lines$matching
                                )
  )
  SE_forest_repeated <- rbind(SE_forest_repeated, 
                              data.frame(
                                line = rep(i, 20), p = 1:20, SE = additional_lines$forest
                              )
  )
  
}

AggForest <- aggregate(SE_forest_repeated, list(SE_forest_repeated$p), mean)
AggMatching <- aggregate(SE_matching_repeated, list(SE_matching_repeated$p), mean)


# Save
count <- 21
png('images/simulation_2_p20_repeated.png', width = 600, height = 600, pointsize = 15)
plot(SE_matching_repeated[1:20, 'SE'], col = 'pink', ylim = c(0, 400), type = 'l', 
     xlab = 'Number of covariates', ylab = 'SE')
lines(SE_forest_repeated[1:20, 'SE'], col ='lightblue')
abline(h = (initial_ate - target_true_ate) ^ 2)
while(count < (nrow(SE_matching_repeated) - 19)) {
  lines(SE_forest_repeated[count:(count+19), 'SE'], col ='lightblue')
  lines(SE_matching_repeated[count:(count+19), 'SE'], col ='pink')
  count <- count + 20
}
lines(AggForest$SE, col = 'blue')
lines(AggMatching$SE, col = 'red')
legend("topright", legend = c('Causal Forest', 'Causal Match'), 
       lty = c(1, 1),
       col = c('blue', 'red'))
dev.off()

###### #######
# Repeated; p = 80

SE_forest_repeated <- data.frame(
  line = rep(1, 80), 
  p = seq(from = 1, to = 80, by = 1), 
  SE = SE_forest[1:80]
)
SE_matching_repeated <- data.frame(
  line = rep(1, 80), 
  p = seq(from = 1, to = 80, by = 1), 
  SE = SE_matching[1:80]
)



# Define the loop as function to simulate the process
loop_random_variables <- function() {
  d0 <- d0[, 1:5]
  d1 <- d1[, 1:3]
  
  covariates <- paste0('x', 2:80)
  target_true_ate <- mean(d1$y1) - mean(d1$y0)
  initial_ate <- mean(d0$y[d0$t == 1]) - mean(d0$y[d0$t == 0])
  # Create an empty vector to capture MSE from Matching
  SE_matching <- numeric()
  # Create an empty vector to capture MSE from Forests
  SE_forest <- numeric()
  # Matching
  SE_matching <- (causalMatchFNN(d1, d0, 'x1') - target_true_ate) ^ 2
  # Forest
  cf <- causalForest(y~x1 , data=d0, treatment=d0$t,
                     split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                     bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L,
                     split.alpha = 0.5, cv.alpha = 0.5,
                     sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                     mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1)
  SE_forest <- (mean(predict(cf, d1)) - target_true_ate) ^ 2

  for (i in covariates) {
    # New random covariates
    d1 <- add_rand(d1, i)
    d0 <- add_rand(d0, i)
    # Matching
    tauPRED_match <- causalMatchFNN(d1, d0, grep('x', colnames(d1), value = T))
    SE_matching_local <- (tauPRED_match - target_true_ate)^2
    SE_matching <- c(SE_matching, SE_matching_local)
    
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
    
    SE_forest <- c(SE_forest, (mean(predict(cf, d1)) - target_true_ate) ^ 2)
    
    print(paste0('Adding covariate: ', i))
    
  }


  return(list(matching = SE_matching, 
              forest = SE_forest))
}


# Now add new lines
for (i in 2:20) {
  additional_lines <- loop_random_variables()
  SE_matching_repeated <- rbind(SE_matching_repeated, 
                                 data.frame(
                                   line = rep(i, 80), p = 1:80, SE = additional_lines$matching
                                   )
                                 )
  SE_forest_repeated <- rbind(SE_forest_repeated, 
                                 data.frame(
                                   line = rep(i, 80), p = 1:80, SE = additional_lines$forest
                                 )
  )
  
}

AggForest <- aggregate(SE_forest_repeated, list(SE_forest_repeated$p), mean)
AggMatching <- aggregate(SE_matching_repeated, list(SE_matching_repeated$p), mean)


# Save
count <- 81
png('images/simulation_2_p80_repeated.png', width = 600, height = 600, pointsize = 15)
plot(SE_matching_repeated[1:80, 'SE'], col = 'pink', ylim = c(0, 190), type = 'l', 
     xlab = 'Number of covariates', ylab = 'SE')
lines(SE_forest_repeated[1:80, 'SE'], col ='lightblue')
while(count < (nrow(SE_matching_repeated) - 79)) {
  lines(SE_forest_repeated[count:(count+79), 'SE'], col ='lightblue')
  lines(SE_matching_repeated[count:(count+79), 'SE'], col ='pink')
  count <- count + 80
}
lines(AggForest$SE, col = 'blue')
lines(AggMatching$SE, col = 'red')
legend("topleft", legend = c('Causal Forest', 'Causal Match'), 
       lty = c(1, 1),
       col = c('blue', 'red'))
dev.off()

