# Libraries and functions
library(ggplot2)
library(scales)
library(knitr)
library(causalTree)
library(dplyr)
source('functions/causalMatchFNN_ties.R')

# Data
df_all <- readRDS('data/gg_clean.Rds')


# Define helper functions
# SE
SE_function <- function(location, tauPred) {
  tauhat_1 <- mean(df_all$y[df_all$d == location & df_all$t == 1]) - mean(df_all$y[df_all$d == location & df_all$t == 0])
  SE <- (tauPred*100 - tauhat_1*100) ^ 2
  return(SE)
}

tauhat_1_function <- function(location) {
  tauhat_1 <- mean(df_all$y[df_all$d == location & df_all$t == 1]) - mean(df_all$y[df_all$d == location & df_all$t == 0])
  return(tauhat_1)
}

# NPE
NPE_function <- function(location) {
  tauhat_1 <- mean(df_all$y[df_all$d == location & df_all$t == 1]) - mean(df_all$y[df_all$d == location & df_all$t == 0])
  
  tauPred_naive <- numeric()
  for (i in unique(df_all$d)[unique(df_all$d) != location]){
    tauPred_naive[i] <- mean(df_all$y[df_all$d == i & df_all$t == 1]) - mean(df_all$y[df_all$d == i & df_all$t == 0])
  }
  
  NPE <- (mean(tauPred_naive) * 100 - tauhat_1 * 100) ^2
  return(NPE)
}

tauPred_naive_function <- function(location) {
  tauPred_naive <- numeric()
  for (i in unique(df_all$d)[unique(df_all$d) != location]){
    tauPred_naive[i] <- mean(df_all$y[df_all$d == i & df_all$t == 1]) - mean(df_all$y[df_all$d == i & df_all$t == 0])
  }
  return(mean(tauPred_naive))
}


### Causal match for all predictions ###

causal_match_scaled <- function(scale_vector = NULL, scale = TRUE, ageVar) {
  if (!is.null(scale_vector)) {
    df_all$age <- rescale(
      ageVar, 
      to = scale_vector
    )
  } else {
    df_all$age <- scale(df_all$age, center = F, scale = scale)
    df_all$voted00 <- scale(df_all$voted00, center = F, scale = scale)
  }
  

  tauPred_match <- numeric()
  
  for (i in unique(df_all$d)) {
    d0 <- df_all[df_all$d != i, ]
    d1 <- df_all[df_all$d == i, ]
    
    tauPred_match <- c(tauPred_match, causalMatchFNN_ties(d1, d0, c('age', 'voted00')))
    print(i)
  }
  names(tauPred_match) <- unique(df_all$d)
  
  return(tauPred_match)
  
}


# Loop the process to obtain results for different scales
specifications <- list(
  list(
    ageVar = df_all$age
  ), 
  list(
    scale_vector = NULL, 
    scale = FALSE, 
    ageVar = df_all$age
  ), 
  list(
    scale_vector = c(0, 1), 
    ageVar = df_all$age
  )
)

# Create latex tables
counter <- 1
for (parameters in specifications) {
  tauPred_match <- do.call("causal_match_scaled", parameters)
  SE <- sapply(names(tauPred_match),  function(x) {
    SE_function(x, tauPred_match[x])
  }, USE.NAMES = F)
  tauhat_1 <- sapply(names(tauPred_match), tauhat_1_function)
  taupred_naive <- sapply(names(tauPred_match), tauPred_naive_function)
  NPE <- sapply(names(tauPred_match),  NPE_function)
  
  table_analysis1_pred_match <- bind_rows(tauPred_match * 100, tauhat_1 * 100, SE, taupred_naive * 100, NPE)
  rownames(table_analysis1_pred_match) <- c(
    '$\\tau_{ITT}^{PRED}$', 
    '$\\hat{\\tau_{ITT}}$', 
    'SE', 
    '$\\tau_{ITT}^{NAIVE}$',
    'NPE'
  )
  
  kable(t(table_analysis1_pred_match), format = 'latex', booktabs = T, digits = 2, escape = F) %>% 
    cat(. , file = sprintf('real_data_analysis/table_analysis_1_match_%s.tex', counter))
  counter <- counter + 1
}


### Causal Forest ###
tauPred_forest <- numeric()

for (i in unique(df_all$d)) {
  d0 <- df_all[df_all$d != i, ]
  d1 <- df_all[df_all$d == i, ]
  
  cf <- causalForest(y ~ age + voted00, data=d0, treatment=d0$t, 
                     split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                     bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                     split.alpha = 0.5, cv.alpha = 0.5,
                     sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                     mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 100,ncolx=2,ncov_sample=2) 
  
  predictioncf <- predict(cf, d1)
  
  tauPred_forest[i] <- mean(predictioncf)
  
  print(i)
}

names(tauPred_forest) <- unique(df_all$d)

# Create latex table
print(tauPred_forest * 100)
SE_f <- sapply(names(tauPred_forest),  function(x) {
  SE_function(x, tauPred_forest[x])
}, USE.NAMES = F)
tauhat_1 <- sapply(names(tauPred_forest), tauhat_1_function)
taupred_naive <- sapply(names(tauPred_forest), tauPred_naive_function)
NPE <- sapply(names(tauPred_forest),  NPE_function)

table_analysis1_pred_forest <- bind_rows(tauPred_forest * 100, tauhat_1 * 100, SE, taupred_naive * 100, NPE)
rownames(table_analysis1_pred_forest) <- c(
  '$\\tau_{ITT}^{PRED}$', 
  '$\\hat{\\tau_{ITT}}$', 
  'SE', 
  '$\\tau_{ITT}^{NAIVE}$',
  'NPE'
)

kable(t(table_analysis1_pred_forest), format = 'latex', booktabs = T, digits = 2, escape = F) %>% 
  cat(. , file = 'real_data_analysis/table_analysis_1_forest.tex')


#### Minnesota ####
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

tauPred_Minneapolis <- causalMatchFNN_ties(df[df$d == 'Minneapolis', ])
