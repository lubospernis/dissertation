library(R6)

  
units_factory <- R6Class(
  "UnitFE", 
  private = list(
    power_rating_watts = 800)
  )

city1 <- units_factory$new
city1
