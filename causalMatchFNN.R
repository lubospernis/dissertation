causalMatchFNN <- function(target, initial, X, seed = NULL) {
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
  
  matched_y_t <- initialwithY_t[indices_t, 'y']
  
  matched_y_t_mean <- mean(matched_y_t)
  
  
  # Select only the individuals according to the Tr indicator from Control
  target_c <- target[target$t == 0, X, drop = F]
  # Select only covariates from the initial location
  initialwithY_c <- initial[initial$t == 0, , drop = F]
  initial_c <- initial[initial$t == 0, X, drop = F]
  
  # second for loop
  indices_c <- knnx.index(initial_c, target_c, k =1)
  
  matched_y_c <- initialwithY_c[indices_c, 'y']
  
  matched_y_c_mean <- mean(matched_y_c)
  
  prediction <- matched_y_t_mean - matched_y_c_mean
  
  # print(
  #   t.test(
  #     matched_y_t, matched_y_c
  #   )
  # )
  
  
  listtoReturn <- list(
    initial_ate = mean(initial$y[initial$t == 1]) - mean(initial$y[initial$t == 0]),
    target_ate = mean(target$y1) - mean(target$y0),
    predicted_ate = prediction
  )
  #print('Done. Success. Matched.')
  return(listtoReturn) 
}

