# This scripts impements the causalMatch procedure
library(tidyr)
# Match each of the observations
match_to_k <- function(target, initial, t = 1, X) {
  # Select only the individuals according to the Tr indicator from Target
  target <- target$dfObserved[target$dfObserved$t == t, X, drop = F]
  # Select only covariates from the initial location
  initialwithY <- initial$dfObserved[initial$dfObserved$t == t, , drop = F]
  initial <- initial$dfObserved[initial$dfObserved$t == t, X, drop = F]
  # Create the list in which all observations are going to be saved
  DistAll <- list()
  # Start loop
  for (i in 1:nrow(target)) {
    # Take the distance from each other observation from the initial dataset
    Distances <- numeric()
    Names <- character()
    for (j in 1:nrow(initial)) {
      TwoRows <- rbind(target[i, , drop = F], initial[j, , drop = F])
      computeDist <- dist(TwoRows)
      toReturn <- as.numeric(computeDist)
      names(toReturn) <- rownames(initial[j, , drop = F])
      Distances[j] <- toReturn
      Names[j] <- names(toReturn)
    }
    
    Distances <- setNames(Distances, Names)
    
    DistAll[[rownames(target[i,,drop=F])]] <- Distances
    
  }
  class(DistAll) <- 'CausalMatchDist'
  attr(DistAll, 'initial.set') <- initialwithY
  attr(DistAll, 'target.set') <- target
  return(DistAll)
}

# Get the outcomes
get_outcomes <- function(CausalMatchDist) {
  if (class(CausalMatchDist) != 'CausalMatchDist') return(NULL)
  
  initial <- attr(CausalMatchDist, 'initial.set')
  ys <- list()
  outcome <- numeric()

  
  for (i in CausalMatchDist) {
    whichYs <- names(which(i == min(i)))
    ys <- append(ys, whichYs)
    y <- mean(initial[rownames(initial) == whichYs, 'y'])
    outcome <- c(outcome, y)
  }
  
  attr(outcome, 'index.list') <- ys
  return(outcome)
  
}

causalMatch <- function(target, initial, X, seed = NULL) {
  target$assignTreatment(seed)
  
  # first for loop
  dist_t <- match_to_k(target, initial, t = 1, X)
  matched_y_t <- get_outcomes(dist_t)
  matched_y_t_mean <- mean(matched_y_t)

    
  # second for loop
  dist_c <- match_to_k(target, initial, t = 0, X)
  matched_y_c <- get_outcomes(dist_c)
  matched_y_c_mean <- mean(matched_y_c)
  
  prediction <- matched_y_t_mean - matched_y_c_mean
  
  
  listtoReturn <- list(
    initial_ate = initial$ate,
    target_ate = target$ate,
    predicted_ate = prediction
  )
  
  attr(listtoReturn, 'prediction.error') <- (prediction - target$ate)^2
  attr(listtoReturn, 'targetinitial.error') <- (initial$ate - target$ate)^2
  print('Done. Success. Matched.')
  return(listtoReturn) 
}

