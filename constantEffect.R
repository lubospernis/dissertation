# Regression model with interactions
model1 <- lm(data = CZ$dfObserved, y ~ t + age + t * age)
summary(model1)

# We see that the point estimate for the interaction effect is close to 0 

# T-tests

# First split the covariate space median and above median
Split <- CZ$dfObserved
Split$split <- ifelse(Split$age > median(Split$age), 1, 0)

t.test(
  Split$y[Split$t == 1 & Split$split == 1] - Split$y[Split$t == 0 & Split$split == 1], 
  Split$y[Split$t == 1 & Split$split == 0] - Split$y[Split$t == 0 & Split$split == 0]
)$stat



# Checking over repeated treatment assignment for covariate splits
tstats <- numeric()
for (i in 1:1000){
  CZ$assignTreatment()
  Split <- CZ$dfObserved
  Split$split <- ifelse(Split$age > median(Split$age), 1, 0)
  
  tstats[i] <- t.test(
    Split$y[Split$t == 1 & Split$split == 1] - Split$y[Split$t == 0 & Split$split == 1], 
    Split$y[Split$t == 1 & Split$split == 0] - Split$y[Split$t == 0 & Split$split == 0]
  )$stat
}

hist(tstats)
summary(tstats)
