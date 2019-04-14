# The causal Match algorithm with ties implemented
# Warning: runs considerably slower than the standard one

causalMatchFNN_ties <- function(target, initial, X, seed = NULL, seed_ties = NULL) {
  # returns the prediction of ATE for the target location
  # the return is the mean of the repeated treatment assignments 
  
  target_predictions <- numeric()
  for (i in 1:100) {
    m <- causalMatchFNNdf_run_once_ties(target, initial, X, seed, seed_ties)
    match_out_ate <- mean(m$y[m$t==1]) - mean(m$y[m$t==0])
    target_predictions[i] <- match_out_ate
  }
  mean_target_predictions <- mean(target_predictions)
  attr(mean_target_predictions, 'all.predictions') <- target_predictions
  return(mean_target_predictions)
}

causalMatchFNNdf_run_once_ties <- function(target, initial, X, seed = NULL, seed_ties = NULL) {
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
  indices_t <- return_knn_indices(target_t, initial_t)
  
  
  matched_y_t <- initialwithY_t[indices_t, ]
  
  
  # Select only the individuals according to the Tr indicator from Control
  target_c <- target[target$t == 0, X, drop = F]
  # Select only covariates from the initial location
  initialwithY_c <- initial[initial$t == 0, , drop = F]
  initial_c <- initial[initial$t == 0, X, drop = F]
  
  # second for loop
  indices_c <- return_knn_indices(target_c, initial_c)
  
  matched_y_c <- initialwithY_c[indices_c, ]
  
  MatchedDF <- rbind(matched_y_t, matched_y_c) 
  
  #print('Done. Success. Matched.')
  return(MatchedDF) 
}

defineCond <- function(knnxObj) {
  # Returns rows which do not have matches of the same distance
  target_rows_to_append <- which(rowMeans(knnxObj$nn.dist) != knnxObj$nn.dist[, 1])
  return(target_rows_to_append)
}

return_knn_indices <- function(target, initial, seed_ties = NULL) {
  # Vector with indices of initial rows
  indices <- numeric()
  
  # Which of the rows from the target locations are already matched
  target_matched <-  numeric()
  
  i <- 2
  while(TRUE){
    k <- get.knnx(initial, target,k = i)
    k_matched <- defineCond(k)
    
    indexmatched <- k_matched[!(k_matched %in% target_matched)]
    
    # If it did not happen to match anything
    if (length(indexmatched) == 0){
      i <- i + 1 
      next
    }
    
     
    choosecolumns <- i - 1
    
    if (i == 2) {
      initial_rows <- k$nn.index[indexmatched, 1:choosecolumns]
    } else {
      initial_rows <- apply(k$nn.index[indexmatched, 1:choosecolumns, drop = F], 1, function(x){
        set.seed(seed_ties)
        sample(x, 1)
      })
    }
    
    indices <- append(indices, initial_rows)
    
    target_matched <- append(target_matched, indexmatched)
    
    # Increase i 
    i <- i + 1
    
    # Exit the loop if a match found for every row in the target dataset
    if (length(target_matched) == nrow(target)) {
      break
    }
  }
  # Reorder in the dataset
  indicesdf <- data.frame(target_row = target_matched, initial_index_row_matched = indices)
  
  indicesdf <- indicesdf[order(indicesdf$target_row), 'initial_index_row_matched']

  return(indicesdf)
 
}