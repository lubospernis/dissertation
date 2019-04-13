calc_SE <- function(target, estimate) {
  ate_true <- mean(target$y1 - target$y0)
  return((estimate - ate_true)^2)
}
calc_NPE <- function(initial, target) {
  tauhatd0 <- mean(initial$y[initial$t==1]) - mean(initial$y[initial$t==0])
  ate_true_target <- mean(target$y1 - target$y0)
  return((tauhatd0 - ate_true_target)^2)
}