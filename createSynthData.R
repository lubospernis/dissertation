# Load the Class generation file
source('synthData_factories.R')

# Create treatment
treatment1 <- treatment_factory$new()
treatment1$effectConstant <- 10 # Define constant treatment effect of 10

# Unit 0
SK <- location_factory$new()
SK$create_sample(500, -3, 'Slovakia', 123) # Draw a random sample and set seed
SK$show_sample_distribution() # Show the distribution
SK$createY0(123) # Create potential outcomes under control
SK$createY1(123) # Create potential otucomes under treatment
SK$ate_true # Report the true ate

SK$assignTreatment(123) # Randomly assign individuals to either control or treatment
SK$ate # Show the experimental ate

# Unit 1
CZ <- location_factory$new()
CZ$create_sample(500, -2, "Czech Republic", 123)
CZ$show_sample_distribution()
CZ$createY0(123)
CZ$createY1(123)
CZ$ate_true
CZ$assignTreatment()
CZ$ate


# In this case the ATE for Czech republic equals the ATE for Slovak republic
# Because the treatment is constant 
CZ$ate_true == SK$ate_true

# Can we retrieve the TRUE ate under repeated sampling? 
ateSave <- numeric()
for (i in 1:1000) {
  CZ$assignTreatment()
  ateSave[i] <- CZ$ate
}

plot(density(ateSave))

# Is there balance on covariates?
balance <- numeric()
for (i in 1:1000) {
  CZ$assignTreatment()
  t <- mean(CZ$dfObserved$x1[CZ$dfObserved$t == 1]) - mean(CZ$dfObserved$x1[CZ$dfObserved$t == 0])
  
  balance[i] <- t
}

plot(density(balance))


