# Libraries and functions
library(ggplot2)
library(scales)
source('functions/causalMatchFNN_ties.R')

# Data
df_all <- readRDS('data/gg_clean.Rds')
# Subset data
df <- df_all[df_all$d == 'St Paul' | df_all$d == 'Minneapolis',]

### Covariate imbalances ###
# Note that we only match on age and voted00

# Age
ggplot(data = df, aes(age, fill = d)) + 
  geom_density(alpha = 0.3)

# Voted00
hist(df$voted00[df$d == 'St Paul'], freq = F)
hist(df$voted00[df$d == 'Minneapolis'], freq = F, add = T, col = alpha('red', 0.3))

### Predictions ###
# First start with rescaling age 
df$age <- rescale(
  df$age, 
  to = c(0, 1)
)

# First, just start using causal match
# For these predictions we only use age and voted00 as predictors
tauPred <- numeric()

for (i in unique(df_all$d)) {
  d0 <- df_all[df_all$d != i, ]
  d1 <- df_all[df_all$d == i, ]
  
  tauPred <- c(tauPred, causalMatchFNN_ties(d1, d0, c('age', 'voted00')))
  print(i)
}

names(tauPred) <- unique(df_all$d)
tauPred * 100

# SE
SE_function <- function(location, tauPred) {
  tauhat_1 <- mean(df_all$y[df_all$d == location & df_all$t == 1]) - mean(df_all$y[df_all$d == location & df_all$t == 0])
  SE <- (tauPred*100 - tauhat_1*100) ^ 2
  return(SE)
}

SE <- sapply(names(tauPred),  function(x) {
  SE_function(x, tauPred[x])
}, 
USE.NAMES = F)

# NPE
NPE_function <- function(location) {
  tauhat_1 <- mean(df_all$y[df_all$d == location & df_all$t == 1]) - mean(df_all$y[df_all$d == location & df_all$t == 0])
  
  tauPred <- numeric()
  for (i in unique(df_all$d)[unique(df_all$d) != location]){
    tauPred[i] <- mean(df_all$y[df_all$d == i & df_all$t == 1]) - mean(df_all$y[df_all$d == i & df_all$t == 0])
  }
  
  NPE <- (mean(tauPred) * 100 - tauhat_1 * 100) ^2
  return(NPE)
}

NPE <- sapply(names(tauPred),  NPE_function)

# Now compare
SE - NPE
