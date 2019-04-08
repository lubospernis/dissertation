Match <- function(target, initial, X, seed = NULL) {
  library(FNN)
  
  m <- knnx.index(initial[, X, drop = F], target[, X, drop = F], k = 1)
  matched <- initial[m, ]
  match_out_ate <- mean(matched$y[matched$t==1]) - mean(matched$y[matched$t==0])
  
  #print('Done. Success. Matched.')
  return(match_out_ate) 
}