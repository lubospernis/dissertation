# creates unlucky and lucky randomisation draws

source('functions/standardMatch.R')
source('functions/causalMatchFNN.R')
library(ggplot2)

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

set.seed(100)
random <- sample(1:nrow(d0))
treat_rows <- random[1:floor(0.5*length(random))]
d0$t <- NA
d0$t[treat_rows] <- 1
d0$t[-treat_rows] <- 0

### Create new var y (realised outcome)
d0$y <- ifelse(d0$t == 1, d0$y1, d0$y0)

# Clean the workspace
rm(random, treat_rows)

# Save as d0_lucky
d0_lucky <- d0

# This should be close to zero
mean(d0_lucky$y1 - d0_lucky$y0) - (mean(d0_lucky$y[d0_lucky$t == 1]) - mean(d0_lucky$y[d0_lucky$t == 0]))

# True ate
lucky_ate <- mean(d0_lucky$y1 - d0_lucky$y0)

# Save this d0 as the lucky case
saveRDS(d0_lucky, 'data/d0_lucky.Rds')
# Save this d1 as the comparision case
saveRDS(d1, 'data/d1.Rds')


### Create the unlucky case
d0_reordered <- d0[order(d0$x1), ]
d0_reordered[1:floor(nrow(d0_reordered) / 2), 't'] <- 1
d0_reordered[-(1:floor(nrow(d0_reordered) / 2)), 't'] <- 0
d0_unlucky <- d0_reordered

# To see how unlucky
ggplot(data = d0_unlucky, aes(x1, fill = factor(t))) + geom_density(alpha = .2) 


# Save the unlucky case
saveRDS(d0_unlucky, 'data/d0_unlucky.Rds')

# see that the true ate did not change
unlucky_ate <- mean(d0_unlucky$y1 - d0_unlucky$y0)

unlucky_ate ==lucky_ate

# hence
true_ate <- unlucky_ate

rm(list = ls())