# Load libraries
library(MatchIt)
library(Matching)

## Setup
# D = 0
HU$dfObserved
# D = 1
BR$dfTarget

## Split the initial location into treatmnent a control
d0_t <- HU$dfObserved[HU$dfObserved$t == 1,]
d0_c <- HU$dfObserved[HU$dfObserved$t == 0,]


carry_out_matching <- function(d0, d1, t) {
  d0$d <- 0
  d1$d <- 1
  d0Ready <- d0[, c(grep("x", colnames(d0)), which(colnames(d0) == 'd')), drop = F]
  d1Ready <- d1[, c(grep("x", colnames(d1)), which(colnames(d1) == 'd')), drop = F]
  
  ready_for_matching <- rbind(d0Ready, d1Ready)
  
  m <- Match(Tr = ready_for_matching$d, 
             X = ready_for_matching[, - (which(colnames(ready_for_matching) == 'd')), drop = F],
             ties = FALSE)
    
  print(t)
  return(m)
  
}

out_t <- carry_out_matching(d0_t, BR$dfTarget, 1)

# These are the treatment rows from the initial location
# Note that there are some observations included more than once
# For now tie-breaking is also set to FALSE
treated <- HU$dfObserved$y[out_t$index.control]

# Now get the control rows from the initial location
out_c <- prepare_dataset_for_matching(d0_c, BR$dfTarget, 0)
control <- HU$dfObserved$y[out_c$index.control]

# Prediction estimate for BR
mean(treated) - mean(control)

# true Brasil
BR$ate

# Can we do this 10000 times?
matching_replication <- function() {
  # Randomly assign treatments
  HU$assignTreatment()
  BR$assignTreatment()
  
  ## Split the initial location into treatmnent a control
  d0_t <- HU$dfObserved[HU$dfObserved$t == 1,]
  d0_c <- HU$dfObserved[HU$dfObserved$t == 0,]
  # Get the treatment rows
  out_t <- carry_out_matching(d0_t, BR$dfTarget, 1)
  
  # These are the treatment rows from the initial location
  # Note that there are some observations included more than once
  # For now tie-breaking is also set to FALSE
  treated <- HU$dfObserved$y[out_t$index.control]
  
  # Now get the control rows from the initial location
  out_c <- carry_out_matching(HU$df, BR$dfTarget, 0)
  control <- HU$dfObserved$y[out_c$index.control]
  
  # Prediction estimate for BR
  ATE_predicted <- mean(treated) - mean(control)
  ATE_real <- BR$ate
  
  
  save <- c(
    ateBrazil = BR$ate, 
    ateHungary = HU$ate, 
    predictedATEforBrazil = ATE_predicted
  )
  
  return(save)
}


asymptoticCheck <- replicate(1000, matching_replication())
asymptoticCheck <- t(asymptoticCheck)
# As a last step what should be done and I did not do, is to regress the matched observations on the covariates
dev.off()
# Plot
hist(asymptoticCheck[,1], col = "green", breaks = 15, xlim = c(-1, 5))
hist(asymptoticCheck[,2], col = "red", add = T , breaks = 15)
hist(asymptoticCheck[,3], col= rgb(0,153/255,153/255,0.8) , add = T, aplha = 0.5, breaks = 15)


### IPW ### - from Stuart et al. 2011

IPW <- function() {
  # First Assign treatments
  HU$assignTreatment()
  BR$assignTreatment()
  
  # Now assign treatments
  treatment <- BR$dfObserved
  control <- HU$dfObserved
  treatment$d <- 1
  control$d <- 0
  
  readyForMatching <- rbind(control, treatment)
  
  # The glm model
  ipwMod <- glm(d~age, family=binomial, data=readyForMatching)
  valuesIpw <- fitted.values(ipwMod)
  readyForMatching$scores <- valuesIpw
  
  # Compute the difference between the two populations
  mean(readyForMatching$scores[readyForMatching$d == 1]) - mean(readyForMatching$scores[readyForMatching$d == 0])
  
  t.test(
    readyForMatching$scores[readyForMatching$d == 1], 
    readyForMatching$scores[readyForMatching$d == 0]
  )
  
  # Now match 
  m_ipw <- Match(Tr = readyForMatching$d, X = readyForMatching$scores, ties = FALSE)
  
  # Do the prediction
  predDf <- readyForMatching[m_ipw$index.control,]
  
  # ATE for Brazil
  saveATE <- mean(predDf$y[predDf$t == 1]) - mean(predDf$y[predDf$t == 0])
  
  save <- c(
    ateBrazil = BR$ate, 
    ateHungary = HU$ate, 
    predictedATEforBrazil = saveATE
  )
  
  return(save)
}


IPWRep <- replicate(1000, IPW())
IPWRep <- t(IPWRep)

hist(IPWRep[,1], col = "green", xlim = c(30, 50), ylim = c(0, 400), breaks = 10)
hist(IPWRep[,2], col = "red", add = T, breaks = 10)
hist(IPWRep[,3], col= rgb(0,153/255,153/255,0.8) , add = T, aplha = 0.5, breaks = 10)


### Causal Forest ###
library(grf)
HU$dfObserved

causalForest <- causal_forest(X = HU$dfObserved[, "age", drop = F], Y = HU$dfObserved$y, W = HU$dfObserved$t, honesty = FALSE)
class(causalForest)

# OOB predictions
predictionsHU <- predict(causalForest)
hist(predictionsHU$predictions)
mean(predictionsHU$predictions)

# Predict to Brazil 
BR_predictions <- predict(causalForest, BR$dfObserved)

mean(BR_predictions$predictions)
BR$ate


## Interpretable ML - when the number of variables is high
library(iml)
predictor = Predictor$new(causalForest, data = BR$dfObserved[, "age", drop = FALSE], y = BR$dfObserved$y)

lime.explain = LocalModel$new(predictor, x.interest = X[1,])
