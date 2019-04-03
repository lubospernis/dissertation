# data generation

Slovakia <- data.frame(
  x1 = c(runif(min= -5, max = 5, n = 400), rnorm(100, mean = -1, sd = 1))
)



# Create country

createCountry <- function(mean) {
  df <- data.frame(
    x1 = c(runif(min= -5, max = 5, n = 400), rnorm(100, mean = mean, sd = 1))
  )
  return(df)
}

disBasic <- runif(min = -5, max = 5, n = 400)
disAdd <- rnorm(n = )