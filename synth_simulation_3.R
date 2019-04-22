# Load helper functions and matching functions
source('functions/helper_Error_calc.R')
source('functions/causalMatchFNN.R')
library(kableExtra)

#############################################################################
### Create a case with big difference in the Z variable between locations ### 
#############################################################################
# This helper function generates y1
generate_y1 <- function(treatment_function) {
  return(y0 + eval(parse(text  = treatment_function)))
}

treatment_function <- '2 + 10 * x1 + z1'

## Create the synth dataset for D = 0 
# Create covariate x1 
set.seed(123) # For reproducibility
uniform <- runif(n = 400, min = 0, max = 10)
set.seed(123)
normal <- rnorm(n = 100, mean = 8)
x1 <- c(uniform, normal)
x1 <- x1[x1 <= 10 & x1 >= 0]
# Create y0 
y0 <- x1
# Create z1
z1 <- 5
# Define treatment
y1 <- generate_y1(treatment_function)

# Bind together to create simulation.1 d= 0 dataset
d0 <- as.data.frame(cbind(x1, z1, y0, y1))

## Create the synth dataset for D = 1 
# Create covariate x1 
set.seed(1234) # For reproducibility
uniform <- runif(n = 400, min = 0, max = 10)
set.seed(1234)
normal <- rnorm(n = 100, mean = 2)
x1 <- c(uniform, normal)
x1 <- x1[x1 <= 10 & x1 >= 0]
# Create y0 
y0 <- x1
# Create z1
z1 <- 20
# Define treatment
y1 <- generate_y1(treatment_function)

# Bind together to create simulation.1 d= 0 dataset
d1 <- as.data.frame(cbind(x1, z1, y0, y1))

# Clean the workspace
rm(x1, y1, y0, uniform, treatment_function, normal, generate_y1)

### Assign treatment to D = 0

set.seed(123)
random <- sample(1:nrow(d0))
treat_rows <- random[1:floor(0.5*length(random))]
d0$t <- NA
d0$t[treat_rows] <- 1
d0$t[-treat_rows] <- 0

### Create new var y (realised outcome)
d0$y <- ifelse(d0$t == 1, d0$y1, d0$y0)

# Clean the workspace
rm(random, treat_rows)


## Get predictions ##
tauPred_m_z <- causalMatchFNN(d1, d0, 'x1')

# Using Forest
cf_z <-  causalForest(y ~ x1,
                      data=d0,
                      treatment=d0$t, 
                      split.Rule="CT",
                      split.Honest=T,
                      split.Bucket=F,
                      bucketNum = 5,
                      bucketMax = 100,
                      cv.option="CT",
                      cv.Honest=T,
                      minsize = 2L, 
                      split.alpha = 0.5,
                      cv.alpha = 0.5,
                      sample.size.total = floor(nrow(d0) / 2),
                      sample.size.train.frac = .5,
                      mtry = ceiling(ncol(d0)/3),
                      nodesize = 3,
                      num.trees= 5,
                      ncolx=length(grep('x', colnames(d0))),
                      ncov_sample= length(grep('x', colnames(d0)))
)

tauPredc_z <- mean(predict(cf_z, d1))

## Create the first row in the table ##

row1 <- data.frame(
  tau_1 = mean(d1$y1-d1$y0), 
  se_forest = calc_SE(d1, tauPredc_z), 
  se_match = calc_SE(d1, tauPred_m_z), 
  NPE = calc_NPE(d0, d1)
)
#############################################################################
## Create a case with small difference in the Z variable between locations ## 
#############################################################################

### Data generation ###

# This helper function generates y1
generate_y1 <- function(treatment_function) {
  return(y0 + eval(parse(text  = treatment_function)))
}

treatment_function <- '2 + 10 * x1 + z1'

## Create the synth dataset for D = 0 
# Create covariate x1 
set.seed(123) # For reproducibility
uniform <- runif(n = 400, min = 0, max = 10)
set.seed(123)
normal <- rnorm(n = 100, mean = 8)
x1 <- c(uniform, normal)
x1 <- x1[x1 <= 10 & x1 >= 0]
# Create y0 
y0 <- x1
# Create z1
z1 <- 5
# Define treatment
y1 <- generate_y1(treatment_function)

# Bind together to create simulation.1 d= 0 dataset
d0 <- as.data.frame(cbind(x1, z1, y0, y1))

## Create the synth dataset for D = 1 
# Create covariate x1 
set.seed(1234) # For reproducibility
uniform <- runif(n = 400, min = 0, max = 10)
set.seed(1234)
normal <- rnorm(n = 100, mean = 2)
x1 <- c(uniform, normal)
x1 <- x1[x1 <= 10 & x1 >= 0]
# Create y0 
y0 <- x1
# Create z1
z1 <- 4
# Define treatment
y1 <- generate_y1(treatment_function)

# Bind together to create simulation.1 d= 0 dataset
d1 <- as.data.frame(cbind(x1, z1, y0, y1))

# Clean the workspace
rm(x1, y1, y0, uniform, treatment_function, normal, generate_y1)

### Assign treatment to D = 0

set.seed(123)
random <- sample(1:nrow(d0))
treat_rows <- random[1:floor(0.5*length(random))]
d0$t <- NA
d0$t[treat_rows] <- 1
d0$t[-treat_rows] <- 0

### Create new var y (realised outcome)
d0$y <- ifelse(d0$t == 1, d0$y1, d0$y0)

# Clean the workspace
rm(random, treat_rows, z1)

tauPred_m_z <- causalMatchFNN(d1, d0, 'x1')

### Predictions ### 

# Using Forest
cf_z <-  causalForest(y ~ x1,
                      data=d0,
                      treatment=d0$t, 
                      split.Rule="CT",
                      split.Honest=T,
                      split.Bucket=F,
                      bucketNum = 5,
                      bucketMax = 100,
                      cv.option="CT",
                      cv.Honest=T,
                      minsize = 2L, 
                      split.alpha = 0.5,
                      cv.alpha = 0.5,
                      sample.size.total = floor(nrow(d0) / 2),
                      sample.size.train.frac = .5,
                      mtry = ceiling(ncol(d0)/3),
                      nodesize = 3,
                      num.trees= 5,
                      ncolx=length(grep('x', colnames(d0))),
                      ncov_sample= length(grep('x', colnames(d0)))
)

tauPredc_z <- mean(predict(cf_z, d1))

### Row 2 in the Table ###
row2 <- data.frame(
  tau_1 = mean(d1$y1-d1$y0), 
  se_forest = calc_SE(d1, tauPredc_z), 
  se_match = calc_SE(d1, tauPred_m_z), 
  NPE = calc_NPE(d0, d1)
)



### Create the final table ###
table1 <- bind_rows(row1, row2)
rownames(table1) <- c(
  "Big difference in Z", 
  "Small difference in Z"
)

colnames(table1) <- c(
  '$\\tau_{1}$', 
  'causal forest', 
  'causalMatch',
  'NPE'
)

kable(table1, format = 'latex', digits = 2, escape = F) %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, " " = 1, "SE" = 2," " = 1)) %T>%
  save_kable('tables/table1', keep_tex = TRUE) 

