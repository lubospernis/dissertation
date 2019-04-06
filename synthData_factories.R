# Install R6 if already not installed
if (!require("R6")) install.packages("R6")
library(R6)

if (!require("httr")) install.packages("httr")
library(httr)

if(!require('magrittr')) install.packages("magrittr")
library(magrittr)

treatment_factory <- R6Class(
  "Treatment", 
  private = list(
    shared = {
      e <- new.env()
      e$effectConstant <- NULL
      e$effectLinear <- NULL
      e$which <- NULL
      e
    }), 
  active = list(
    #' Create an average treatment effect.
    #' 
    #' @param value The value of the ATE.
    #' @examples
    #' $effect <- 4
    effectConstant = function(value) {
      if(missing(value)){
        private$shared$effectConstant
      } else {
        private$shared$effectConstant <- value
        private$shared$which <- 'effectConstant'
      }
    }, 
    effectLinear = function(value) {
      if(missing(value)){
        private$shared$effectLinear
      } else {
        private$shared$effectLinear <- value
        private$shared$which <- 'effectLinear'
      }
    }
  )
)

location_factory <- R6Class(
  "Location", 
  inherit = treatment_factory,
  private = list(
    ..sampleD = NULL,
    ..sample = NULL,
    ..y0 = NULL,
    ..y1 = NULL,
    ..df = NULL,
    ..sampleS = NULL, 
    ..countryName = NULL
  ), 
  public = list(
    create_sample = function(n, mean, name, seed = NULL) {
      set.seed(seed)
      basic <- runif(min = 0, max = 10, n = 0.8 * n)
      add <- rnorm(n = 0.2 * n, mean, sd = 0.1)
      add <- add[add <= 10 | add >= 0]
      
      # save as sample
      df <- data.frame(
        x1 = c(basic, add)
      )
      
      private$..df <- df
      private$..countryName <- name
    
    },
    create_covariate = function(name, random = TRUE, seed = NULL) {
      # How should this work?
      # Either say random that is the simplest case or in a certain relationship
      # Based on real data
      # For example the way education could work that it would intelligently distribute
      # The values
      if (random != TRUE) return('Currently, you can only create a random covariate')
      set.seed(seed)
      randCov <- rnorm(nrow(private$..df), mean = 5)
      
      private$..df[, name] <- randCov
      print('Success...')
      
    },
    show_sample_distribution = function() {
      if (length(private$..df$x1) != 0) {
        hist(private$..df$x1, 
             main = private$..countryName, 
             sub = "Distribution of a covariate x1", 
             xlab = "x1")
      }
    }, 
    createY0 = function(seed = NULL) {
      if (!is.null(seed)) set.seed(seed)
      y0 <- private$..df$x1 * 0.5 + rnorm(length(private$..df$x1), sd= 0)
      
      # save to df
      private$..df[, 'y0'] <- y0

    }, 
    createY1 = function(seed = NULL) {
      if (!is.null(seed)) set.seed(seed)
      
      if (private$shared$which == "effectConstant") {
        private$..df$y1 <- private$..df$y0 + 
          rnorm(n = nrow(private$..df), mean = private$shared$effectConstant, sd = 0)
      } else if (private$shared$which == 'effectLinear') {
        # First parse
        for (i in colnames(private$..df)) {
          pattern <- paste0('-', i, '-')
          private$shared$effectLinear <- gsub(pattern, paste0("private$..df[, '", i, "']"), private$shared$effectLinear)
        }
        private$..df$y1 <- eval(parse(text = private$shared$effectLinear))
      }

    },
    assignTreatment = function(seed = NULL){
      if (!is.null(seed)) set.seed(seed)
      
      
      df <- private$..df
      set.seed(seed)
      randnums <- sample(1:nrow(private$..df))
      
      pickhalf <- randnums[1:(0.5 * length(randnums))]
      
      df$t <- NA
      df$t[pickhalf] <- 1
      df$t[-pickhalf] <- 0
      
      df$y <- ifelse(df$t == 1, df$y1, df$y0)
      

      private$..df <- df
    }
  ),
  active = list(
    y0 = function(){
      private$..df$y0
    }, 
    y1 = function(){
      private$..df$y1
    }, 
    ate_true = function() {
      mean(private$..df$y1) - mean(private$..df$y0)
    },
    ate = function() {
      mean(private$..df$y[private$..df$t == 1]) - mean(private$..df$y[private$..df$t == 0]) 
    },
    df = function() {
      private$..df
    },
    dfObserved = function() {
      private$..df[,-which(names(private$..df) %in% c("y0","y1"))]
    },
    dfTarget = function() {
      private$..df[, -which(names(private$..df) %in% c("y0","y1", "y", "t")), drop = FALSE]
    },
    sample = function() {
      private$..sampleD
    }
  )
) 

