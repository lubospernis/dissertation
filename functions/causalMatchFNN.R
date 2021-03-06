causalMatchFNNdf_run_once <- function(target, initial, X, seed = NULL) {
  library(FNN)
  
  # Assign treatment to target - if seed is not null, standardize
  set.seed(seed)
  random <- sample(1:nrow(target))
  treat_rows <- random[1:floor(0.5*length(random))]
  target$t <- NA
  target$t[treat_rows] <- 1
  target$t[-treat_rows] <- 0
  
  # Select only the individuals according to the Tr indicator from Target
  target_t <- target[target$t == 1, X, drop = F]
  # Select only covariates from the initial location
  initialwithY_t <- initial[initial$t == 1, ,drop = F]
  initial_t <- initial[initial$t == 1, X, drop = F]
  # first for loop
  indices_t <- knnx.index(initial_t, target_t, k =1)
  matched_y_t <- initialwithY_t[indices_t, ]

  
  # Select only the individuals according to the Tr indicator from Control
  target_c <- target[target$t == 0, X, drop = F]
  # Select only covariates from the initial location
  initialwithY_c <- initial[initial$t == 0, , drop = F]
  initial_c <- initial[initial$t == 0, X, drop = F]
  
  # second for loop
  indices_c <- knnx.index(initial_c, target_c, k =1)

  matched_y_c <- initialwithY_c[indices_c, ]
 
  MatchedDF <- rbind(matched_y_t, matched_y_c) 
 
  #print('Done. Success. Matched.')
  return(MatchedDF) 
}


causalMatchFNN <- function(target, initial, X, seed = NULL) {
  # returns the prediction of ATE for the target location
  # the return is the mean of the repeated treatment assignments 
  
  target_predictions <- numeric()
  
  for (i in 1:100) {
    m <- causalMatchFNNdf_run_once(target, initial, X, seed)
    match_out_ate <- mean(m$y[m$t==1]) - mean(m$y[m$t==0])
    target_predictions[i] <- match_out_ate
  }
  
  return(mean(target_predictions))
}
