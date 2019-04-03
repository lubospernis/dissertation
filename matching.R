# Matching
library(MatchIt)
library(Matching)

# Separetely match the treated and then match the control
# and then substract control from treatment to get an ate prediction

# So how do we match? 

# We follow Hotz (2005) 


# Create a function does it work?
Matching <- function() {
  # First Assign treatments
  HU$assignTreatment()
  BR$assignTreatment()
  
  # Now Bind together for matching
  treatment <- BR$dfObserved
  control <- HU$dfObserved
  treatment$d <- 1
  control$d <- 0
  
  readyForMatching <- rbind(control, treatment)
  
  m <- Match(Tr = readyForMatching$d, X = readyForMatching$x1, ties = FALSE)
  
  # Do the prediction
  predDf <- readyForMatching[m$index.control,]
  
  # ATE for Brazil
  saveATE <- mean(predDf$y[predDf$t == 1]) - mean(predDf$y[predDf$t == 0])
  
  # Now show the real ATE for Brazil
  save <- c(
    ateBrazil = BR$ate, 
    ateHungary = HU$ate, 
    predictedATEforBrazil = saveATE
  )
  return(save)
}

# Check the results from matching 
MatchingRep <- replicate(1000, Matching())
MatchingRep <- t(MatchingRep)

# As a last step what should be done and I did not do, is to regress the matched observations on the covariates
dev.off()
# Plot
hist(MatchingRep[,1], col = "green", breaks = 15)
hist(MatchingRep[,2], col = "red", add = T , breaks = 15)
hist(MatchingRep[,3], col= rgb(0,153/255,153/255,0.8) , add = T, aplha = 0.5, breaks = 15)


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
