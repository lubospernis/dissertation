# examples from match causal
source('causalMatch.R')
library(tidyr)

# TESTING
Mystery <- location_factory$new()
Known <- location_factory$new()
treatment <- treatment_factory$new()
treatment$effectLinear <- '4 + -x1-'
Mystery$create_sample(n= 10, mean = 8, 'mystery')
Known$create_sample(n = 10, mean = 1, 'known')
Mystery$createY0()
Mystery$createY1()
Known$createY0()
Known$createY1()
Mystery$assignTreatment()
Known$assignTreatment()


Known$show_sample_distribution()
Mystery$show_sample_distribution()


pred <- causalMatch(Mystery, Known, 'x1')
attr(pred, 'prediction.error')
Mystery$ate
Known$ate

# if I replicate this thing enough times - do we get an asymptotit behaviour
many_predictions <- replicate(100, causalMatch(Mystery, Known, 'x1'))

many_predictions_df <-data.frame(t(many_predictions))

many_predictions_df <- unnest(many_predictions_df)

pred_error <- apply(many_predictions_df, 1, function(x) {
  (x[3] - x[2]) ^ 2
})

target_error <- apply(many_predictions_df, 1, function(x) {
  (x[2] - x[1]) ^ 2
})

mean(pred_error) < mean(target_error)

hist(pred_error)

hist(many_predictions_df$target_ate)
abline(v = unique(many_predictions_df$initial_ate), col = 'blue')
hist(many_predictions_df$predicted_ate, col = 'green', aplha = 0.8, add= T)

# The problem here is that I could have picked just the lucky outcome

# What happens if we also reshuffle the initial treatment assignment
# repeated stuff

repeated <- function(initial, target){
  initial$assignTreatment()
  many_predictions <- replicate(100, causalMatch(target, initial, 'x1'))
  
  many_predictions_df <-data.frame(t(many_predictions))
  library(tidyr)
  many_predictions_df <- unnest(many_predictions_df)
  
  pred_error <- apply(many_predictions_df, 1, function(x) {
    (x[3] - x[2]) ^ 2
  })
  
  target_error <- apply(many_predictions_df, 1, function(x) {
    (x[2] - x[1]) ^ 2
  })
  
  logi <- mean(pred_error) < mean(target_error)
  mean_pred_error <- mean(pred_error) 
  mean_target_error <- mean(target_error)
  
  best_prediction_estimate <- mean(many_predictions_df$predicted_ate)
  best_target_ate <- mean(many_predictions_df$target_ate)
  print(logi)
  return(c(mean_pred_error, mean_target_error))
  
}

replication_init <- replicate(100, repeated(Known, Mystery))
toplot <- as.data.frame(t(replication_init))
colnames(toplot) <- c('pred error', 'target init error')
hist(toplot$`pred error`, breaks= 100, col = 'green', xlim = c(0, 6))
hist(toplot$`target init error`, breaks = 100, col = 'red', add = T)
