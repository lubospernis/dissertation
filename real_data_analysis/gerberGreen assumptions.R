# Gerber and Green data 
# https://isps.yale.edu/research/data/d017

library(ggplot2)
# Load dataset
gg <- readRDS('data/gg_clean.Rds')


### Assumptions ###
# Overlap Assumption Age
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
overlapdf_age <- as.data.frame(overlapMatrix)
overlapdf[rowMeans(overlapdf_age) > 0 & apply(overlapdf_age, 1, min) == 0, ]


# Overlap Assumption Voted00
min_rep <- min(gg$age)
max_rep <- max(gg$age)
locations <- unique(gg$d)
overlapMatrix <- matrix(NA, ncol = 5, nrow = max_rep)
colnames(overlapMatrix) <- locations
for (i in min_rep:max_rep){
  for (j in locations) {
    overlapMatrix[i, j] <- min(length(gg$voted00[gg$voted00 == 0 & gg$d == j & gg$age == i]), length(gg$voted00[gg$voted00 == 1 & gg$d == j & gg$age == i]))
  }
}
overlapdf_voted <- as.data.frame(overlapMatrix)
overlapdf[rowMeans(overlapdf_voted) > 0 & apply(overlapdf_voted, 1, min) == 0, ]

