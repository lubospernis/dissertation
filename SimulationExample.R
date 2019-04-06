# Load the factories object
source('synthData_factories.R')
source('causalMatchFNN.R')

# Load additional libraries
library(causalTree)

# Treatment
t1 <- treatment_factory$new()
t1$effectLinear <- '4 + 10* -x1-'


# Create locations
S0 <- location_factory$new()
S0$create_sample(500, mean = 7, name = 's0')
S0$createY0()
S0$createY1()
S0$assignTreatment()
S0$ate
S1 <- location_factory$new()
S1$create_sample(500, mean = 1, name = 's1')
S1$createY0()
S1$createY1()
S1$assignTreatment()
S1$ate


# Knn
match <- causalMatch(S1, S0, 'x1')

# Causal Forest
cf <- causalForest(y ~ x1, data=S0$dfObserved, treatment=S0$dfObserved$t, 
                   split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                   bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                   split.alpha = 0.5, cv.alpha = 0.5,
                   sample.size.total = floor(nrow(S0$dfObserved) / 2), sample.size.train.frac = .5,
                   mtry = ceiling(ncol(S0$dfObserved)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1) 

predictioncf <- predict(cf, S1$dfTarget)
# pred error
(mean(predictioncf) - S1$ate) ^2
##### SIMULATIONS #######

### Forests
replicateForests <- function(initial, target, formula) {
  initial$assignTreatment()
  
  cf <- causalForest(formula, data=initial$dfObserved, treatment=initial$dfObserved$t, 
                     split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                     bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                     split.alpha = 0.5, cv.alpha = 0.5,
                     sample.size.total = floor(nrow(initial$dfObserved) / 2), sample.size.train.frac = .5,
                     mtry = ceiling(ncol(initial$dfObserved)/3), nodesize = 3, num.trees= 5,ncolx=1,ncov_sample=1) 
  
  predictioncf <- predict(cf, target$dfTarget)
  # pred error
  predE <- (mean(predictioncf) - target$ate) ^2
  # trueE 
  trueE <- (initial$ate - target$ate)^2
  
  return(c(predE, trueE))
  
}

forestError <- replicate(100, replicateForests(S0, S1, y ~ x1))
hist(t(forestError)[, 1], breaks = 20, main = 'causal Forest', xlab = 'Pred. Error', xlim = c(0, 200))
hist(t(forestError)[, 2], breaks = 20, col = 'red', add = T)

### Match
replicateMatch <- function(initial, target, covariate){
  initial$assignTreatment()
  match <- causalMatchFNN(target, initial, covariate)
  pred_error <- attr(match, 'prediction.error')
  true_error <- attr(match, 'targetinitial.error')
  return(c(pred_error, true_error))
}

matchError <- replicate(100, replicateMatch(S0, S1, 'x1'))
hist(t(matchError)[, 1], breaks = 20, main = 'causal Match', xlab = 'Pred. Error', xlim = c(0, 200))
hist(t(matchError)[, 2], breaks = 20, col = 'red', add = T)

# Compare forest and Match
mean(t(forestError)[, 1])

mean(t(matchError)[, 1])
