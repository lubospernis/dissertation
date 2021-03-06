# Libraries and functions
library(ggplot2)
library(scales)
library(knitr)
library(causalTree)
library(dplyr)
source('functions/causalMatchFNN_ties.R')
source('functions/createBiGraph.R')

# Data
df_all <- readRDS('data/gg_clean.Rds')


### Assumptions ###
# Overlap Assumption Age
min_rep <- min(df_all$age)
max_rep <- max(df_all$age)
locations <- unique(df_all$d)
overlapMatrix <- matrix(NA, ncol = 5, nrow = max_rep)
colnames(overlapMatrix) <- locations
for (i in min_rep:max_rep){
  for (j in locations) {
    overlapMatrix[i, j] <- length(df_all$age[df_all$age == i & df_all$d == j])
  }
}
overlapdf_age <- as.data.frame(overlapMatrix)
overlapdf[rowMeans(overlapdf_age) > 0 & apply(overlapdf_age, 1, min) == 0, ]


# Overlap Assumption Voted00
min_rep <- min(df_all$age)
max_rep <- max(df_all$age)
locations <- unique(df_all$d)
overlapMatrix <- matrix(NA, ncol = 5, nrow = max_rep)
colnames(overlapMatrix) <- locations
for (i in min_rep:max_rep){
  for (j in locations) {
    overlapMatrix[i, j] <- min(length(df_all$voted00[df_all$voted00 == 0 & df_all$d == j & df_all$age == i]), length(df_all$voted00[df_all$voted00 == 1 & df_all$d == j & df_all$age == i]))
  }
}
overlapdf_voted <- as.data.frame(overlapMatrix)
overlapdf[rowMeans(overlapdf_voted) > 0 & apply(overlapdf_voted, 1, min) == 0, ]



### Define helper functions ###
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

# Create latex tables for various specifications of causal match
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
tauhat_1_f <- sapply(names(tauPred_forest), tauhat_1_function)
taupred_naive_f <- sapply(names(tauPred_forest), tauPred_naive_function)
NPE_f <- sapply(names(tauPred_forest),  NPE_function)

table_analysis1_pred_forest <- bind_rows(tauPred_forest * 100, tauhat_1_f * 100, SE_f, taupred_naive_f * 100, NPE_f)
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

# Scale
df$age <- rescale(
  df$age, 
  to = c(0, 1)
)

### Predictions ###
# Let D = 1 be St Paul and D = 0 Minneapolis
d1 <- df[df$d == 'St Paul', ]
d0 <- df[df$d == 'Minneapolis', ]

# Visualise matching
create_bipartite_graph(d1, d0, c('age', 'voted00'), name = 'd1_stpaul_d0_minneapolis')

tauPred_StPaul <- causalMatchFNN_ties(d1, d0, c('age', 'voted00'))

cf_StPaul <- causalForest(y ~ age + voted00, data=d0, treatment=d0$t, 
                   split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                   bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                   split.alpha = 0.5, cv.alpha = 0.5,
                   sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                   mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 100,ncolx=2,ncov_sample=2) 

predictioncf_StPaul <- predict(cf_StPaul, d1)

tauPred_StPaul_forest <- mean(predictioncf_StPaul)

# Let D = 1 be Minneapolis and D = 0 St Paul
d1 <- df[df$d == 'Minneapolis', ]
d0 <- df[df$d == 'St Paul',]

# Visualise matching
create_bipartite_graph(d1, d0, c('age', 'voted00'), name = 'd1_minneapolis_d0_stpaul')


tauPred_Minneapolis <- causalMatchFNN_ties(d1, d0, c('age', 'voted00'))

cf_Minneapolis <- causalForest(y ~ age + voted00, data=d0, treatment=d0$t, 
                          split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                          bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                          split.alpha = 0.5, cv.alpha = 0.5,
                          sample.size.total = floor(nrow(d0) / 2), sample.size.train.frac = .5,
                          mtry = ceiling(ncol(d0)/3), nodesize = 3, num.trees= 100,ncolx=2,ncov_sample=2) 

predictioncf_Minneapolis <- predict(cf_Minneapolis, d1)

tauPred_Minneapolis_forest <- mean(predictioncf_Minneapolis)


# Are both of the predictions lower than the tauhat_1 for the location? 
tauPred_Minneapolis < tauhat_1_function('Minneapolis')
tauPred_StPaul < tauhat_1_function('St Paul')

# Latex table for Causal Match
tauPred_minnesota <- c(tauPred_Minneapolis, tauPred_StPaul)
names(tauPred_minnesota) <- c('Minneapolis', 'St. Paul')

tauhat_1_minnesota <- c(
  tauhat_1_function('Minneapolis'), 
  tauhat_1_function('St Paul')
)
names(tauhat_1_minnesota) <- c('Minneapolis', 'St. Paul')

SE_minnesota <- c(SE_function('Minneapolis', tauPred_Minneapolis), 
                  SE_function('St Paul', tauPred_StPaul)
                  )
names(SE_minnesota) <- c('Minneapolis', 'St. Paul')

taupred_naive_minnesota <- c(
  tauhat_1_function('St Paul'), 
  tauhat_1_function('Minneapolis')
)

names(taupred_naive_minnesota) <- c('Minneapolis', 'St. Paul')

NPE_minnesota <- c(
  (tauhat_1_function('St Paul')*100 - tauhat_1_function('Minneapolis')*100) ^2, 
  (tauhat_1_function('Minneapolis')*100 - tauhat_1_function('St Paul')*100)^2
)

names(NPE_minnesota) <- c('Minneapolis', 'St. Paul')

table_analysis1_pred_minnesota <- bind_rows(tauPred_minnesota * 100,
                                            tauhat_1_minnesota * 100,
                                            SE_minnesota,
                                            taupred_naive_minnesota * 100,
                                            NPE_minnesota)
rownames(table_analysis1_pred_minnesota) <- c(
  '$\\tau_{ITT}^{PRED}$', 
  '$\\hat{\\tau_{ITT}}$', 
  'SE', 
  '$\\tau_{ITT}^{NAIVE}$',
  'NPE'
)

kable(t(table_analysis1_pred_minnesota), format = 'latex', booktabs = T, digits = 2, escape = F) %>% 
  cat(. , file = 'real_data_analysis/table_analysis_1_minnesota_match.tex')


# Latex table for Causal Match
tauPred_minnesota <- c(tauPred_Minneapolis_forest, tauPred_StPaul_forest)
names(tauPred_minnesota) <- c('Minneapolis', 'St. Paul')

tauhat_1_minnesota <- c(
  tauhat_1_function('Minneapolis'), 
  tauhat_1_function('St Paul')
)
names(tauhat_1_minnesota) <- c('Minneapolis', 'St. Paul')

SE_minnesota <- c(SE_function('Minneapolis', tauPred_Minneapolis_forest), 
                  SE_function('St Paul', tauPred_StPaul_forest)
)
names(SE_minnesota) <- c('Minneapolis', 'St. Paul')

taupred_naive_minnesota <- c(
  tauhat_1_function('St Paul'), 
  tauhat_1_function('Minneapolis')
)

names(taupred_naive_minnesota) <- c('Minneapolis', 'St. Paul')

NPE_minnesota <- c(
  (tauhat_1_function('St Paul')*100 - tauhat_1_function('Minneapolis')*100) ^2, 
  (tauhat_1_function('Minneapolis')*100 - tauhat_1_function('St Paul')*100)^2
)

names(NPE_minnesota) <- c('Minneapolis', 'St. Paul')

table_analysis1_pred_minnesota <- bind_rows(tauPred_minnesota * 100,
                                            tauhat_1_minnesota * 100,
                                            SE_minnesota,
                                            taupred_naive_minnesota * 100,
                                            NPE_minnesota)
rownames(table_analysis1_pred_minnesota) <- c(
  '$\\tau_{ITT}^{PRED}$', 
  '$\\hat{\\tau_{ITT}}$', 
  'SE', 
  '$\\tau_{ITT}^{NAIVE}$',
  'NPE'
)

kable(t(table_analysis1_pred_minnesota), format = 'latex', booktabs = T, digits = 2, escape = F) %>% 
  cat(. , file = 'real_data_analysis/table_analysis_1_minnesota_forest.tex')


