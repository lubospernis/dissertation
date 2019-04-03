# My own version of matching

# First I define the function which is the Euclidian distance

#D = 1 
BR$dfTarget
#D = 0
HU$dfObserved

# So I want to say this

# For every i in Brazil I want to find k similar treated units in Hungary and k similar control units in Hungary

create_dist_matrix <- function(rowNumber = 1, distance = 'euclidian', target, initial, covariates, treatment) {
  target_line_one <- target$dfTarget[rowNumber, covariates, drop  = F]
  
  initial_split <- initial$dfObserved[initial$dfObserved$t == treatment, ]
  
  initial_covariates <- initial_split[, covariates, drop = F]
  rownames(target_line_one) <- 99999999999
  bind_together <- rbind(target_line_one, initial_covariates)
  
  dist_matrix <- dist(bind_together, diag = T, upper = T)
  dist_matrix <- as.matrix(dist_matrix)
  
  
  return(dist_matrix[, 1])
}



save <- create_dist_matrix(2, target = BR, initial = HU, covariates = 'x1', treatment = 0)

match_minimum_k <- function(k = 1, input) {
  # input should the output from create_dist_matrix
  save_without_first <- input[-1]
  sorted <- sort(save_without_first)
  toReturn <- list(matchValues = sorted[1:k], 
                   matchNamesofRows = names(sorted[k]))
  
  return(toReturn)
}

val <- match_minimum_k(input = save)
val$matchNamesofRows

sort(save)[1]

# Now I get rid of thr first one
save_without_first <- save[-1]
sort(save_without_first)[1]

HU$dfObserved$x1[rownames(HU$dfObserved) == "128"]
BR$dfTarget$x1[2]

# So the next step is actually doing the matching and saving the results from it
perform_knn_matching <- function(k = 1, target, initial, covariates) {
  treatments <- numeric()
  for (i in 1:nrow(target$dfTarget)) {
    # Treatment
    
    get_dist <- create_dist_matrix(rowNumber = i, target = target, initial = initial, covariates = covariates, treatment = 1)
    get_match <- match_minimum_k(k = k, get_dist)
    
    # Get the Value from initial
    Value <- initial$dfObserved$x1[rownames(initial$dfObserved) == get_match$matchNamesofRows]
    
    treatments[i] <- Value
    # Control
    
    
    
  
  }
  
  return(treatments)
  
}

save <- perform_knn_matching(k = 5, target = BR, initial = HU, 'x1')
mean(save)
