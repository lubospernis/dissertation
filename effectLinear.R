source('synthData_factories.R')

# Situation of heterogenous treatment effects depending on the value of age

HU <- location_factory$new()
HU$create_pop('Hungary', 2000)
HU$create_sample(1000, 123)
HU$createY0(123)
# Create new treatment
treatment2 <- treatment_factory$new()
# Specify the effect
treatment2$effectLinear <- '-y0- + 10*log(-age- + 0.1) + 5'
# Create Y1
HU$createY1(123)
# Assign treatment
HU$assignTreatment(123)
# Only work with the observed files
HU$dfObserved
HU$ate

## Testing heterogeneity
# Interaction effects - so yes
model1 <- lm(data = HU$dfObserved, y ~ t * age)
summary(model1)

###  Now generalise - start from the Hotz paper ### 
# We want to generalise to Brazil
BR <- location_factory$new()
BR$create_pop('Brazil', 2000)
BR$create_sample(1000, 123)
BR$createY0(123)
BR$createY1(123)
BR$assignTreatment(123)

# Compare Brazil to Hungary
par(mfrow = c(1, 2))
HU$show_sample_distribution()
BR$show_sample_distribution()
dev.off()
# Matching
library(MatchIt)
library(Matching)


# Create a function does it work?
doesItWork <- function() {
  # First Assign treatments
  HU$assignTreatment()
  BR$assignTreatment()
  # Now Bind together for matching
  treatment <- BR$dfObserved
  control <- HU$dfObserved
  treatment$d <- 1
  control$d <- 0
  
  readyForMatching <- rbind(control, treatment)
  
  m <- Match(Tr = readyForMatching$d, X = readyForMatching$age, ties = FALSE)

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
results <- list()
for (i in 1:1000) {
  results[[i]] <- doesItWork()
}

resultsDf <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T))
colnames(resultsDf) <- c("ATE_Brazil", "ATE_Hungary", "Predicted_Brazil")

# As a last step what should be done and I did not do, is to regress the matched observations on the covariates

# Plot
hist(resultsDf$ATE_Brazil, col = "green", xlim = c(30, 50), ylim = c(0, 400))
hist(resultsDf$ATE_Hungary, col = "red", add = T)
hist(resultsDf$Predicted_Brazil, col= rgb(0,153/255,153/255,0.8) , add = T, aplha = 0.5)


### IPW ### - from Stuart et al. 2011

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
