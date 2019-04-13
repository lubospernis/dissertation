# Gerber and Green data 
# https://isps.yale.edu/research/data/d017

library(ggplot2)
# Load dataset
gg <- readRDS('data/gg_clean.Rds')


### Assumptions ###
# Overlap Assumption
min_rep <- min(gg$age)
max_rep <- max(gg$age)
locations <- unique(gg$d)
overlapMatrix <- matrix(NA, ncol = 5, nrow = max_rep)
colnames(overlapMatrix) <- locations
for (i in min_rep:max_rep){
  for (j in locations) {
    overlapMatrix[i, j] <- length(gg$age[gg$age == i & gg$d == j])
  }
}
overlapdf <- as.data.frame(overlapMatrix)
overlapdf[rowMeans(overlapdf) > 0 & apply(overlapdf, 1, min) == 0, ]


