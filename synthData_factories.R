# Install R6 if already not installed
if (!require("R6")) install.packages("R6")
library(R6)

if (!require("httr")) install.packages("httr")
library(httr)

treatment_factory <- R6Class(
  "Treatment", 
  private = list(
    shared = {
      e <- new.env()
      e$effect <- NULL
      e
    }), 
  active = list(
    #' Create an average treatment effect.
    #' 
    #' @param value The value of the ATE.
    #' @examples
    #' $effect <- 4
    effect = function(value) {
      if(missing(value)){
        private$shared$effect
      } else {
        private$shared$effect <- value
      }
    }
  )
)

location_factory <- R6Class(
  "Location", 
  inherit = treatment_factory,
  private = list(
    ..pop = NULL,
    ..sampleD = NULL,
    ..sample = NULL,
    ..y0 = NULL,
    ..y1 = NULL,
    ..df = NULL,
    ..sampleS = NULL, 
    ..countryName = NULL
  ), 
  public = list(
    create_pop = function(country, year) {
      # GET request 
      url <- 'http://api.population.io:80/1.0/population/'
      finalurl <- paste0(url, year,  '/', URLencode(country), '/')
      r <- GET(url = finalurl)
      str <- content(r)
      # Successful?
      print(r$status_code)
      # Check
      if (r$status_code != 200) stop('The request is malformed')
      # In data frame 
      df <- data.frame(matrix(unlist(str), nrow=length(str), byrow=T))
      df$X6 <- as.numeric(as.character(df$X6))
      # Save distrib
      private$..pop <- df
      # Save country Name
      private$..countryName <- country
    },
    create_sample = function(n, seed = NULL) {
      if (is.null(private$..pop)) {
        print('Please first create a population.')
      } else {
        # Save all observations as a list
        listD <- apply(private$..pop, 1, function(x){
          rep(x['X3'], x['X6'])
        })
        # Change to vector
        vectorD <- unlist(listD)
        # Sample N
        if (!is.null(seed)) set.seed(seed)
        sampleD <- sample(vectorD, n) %>% as.numeric()
        # Save Sample Distrib
        private$..sampleD <- sampleD
        # Save 
        sample <- data.frame(table(sampleD))
        colnames(sample) <- c('Age', 'Freq')
        private$..sample <- sample
        
        # Save sample size 
        private$..sampleS <- n
      }
    },
    show_sample_distribution = function() {
      if (length(private$..sampleD) != 0) {
        hist(private$..sampleD, 
             main = private$..countryName, 
             sub = "Age distribution", 
             xlab = "Age")
      }
    }, 
    createY0 = function(seed = NULL) {
      if (!is.null(seed)) set.seed(seed)
      distrib <- apply(private$..sample, 1, function(x) {
        abs(rnorm(n = as.numeric(x['Freq']), mean = as.numeric(x['Age']), sd = 0))
      })
      private$..y0 <- unlist(distrib)
    }, 
    createY1 = function(seed = NULL) {
      if (!is.null(seed)) set.seed(seed)
      private$..y1 <- private$..y0 + 
        rnorm(n = private$..sampleS, mean = private$shared$effect, sd = 0)
    },
    assignTreatment = function(seed = NULL){
      if (!is.null(seed)) set.seed(seed)
      set.seed(seed)
      df <- data.frame(
        age = private$..sampleD, 
        y0 = private$..y0, 
        y1 = private$..y1
      )
      
      df$random <- sample(1:private$..sampleS)
      
      df <- df[order(df$random), ]
      df$t <- c(rep(1, 0.5 * nrow(df)), rep(0, nrow(df) - 0.5 * nrow(df)))
      
      df$y <- ifelse(df$t == 1, df$y1, df$y0)
      
      df$random <- NULL
      
      private$..df <- df
    }
  ),
  active = list(
    pop = function(){
      private$..pop
    }, 
    y0 = function(){
      private$..y0
    }, 
    y1 = function(){
      private$..y1
    }, 
    ate_true = function() {
      mean(private$..y1) - mean(private$..y0)
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
    sample = function() {
      private$..sampleD
    }
  )
) 

