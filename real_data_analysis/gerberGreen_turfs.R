# Load libraries
library(knitr)
library(magrittr)

# Code taken from the code file of GreenGerberNickerson

# Turf vs. no turf
data <- read.csv("data/GreenGerberNickerson_JP_2003.csv")

# Sorting turf in proper alphabetical order.
data$turf = as.character(data$turf)
data$turf[nchar(data$turf) == 1] = paste("000", data$turf[nchar(data$turf) == 1], sep = "")
data$turf[nchar(data$turf) == 2] = paste("00", data$turf[nchar(data$turf) == 2], sep = "")
data$turf[nchar(data$turf) == 3] = paste("0", data$turf[nchar(data$turf) == 3], sep = "")

# Building cityturf variable.
data$cityturf = as.factor(paste(data$city, data$turf, sep = "_"))

# Column 2:
lm2.b = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "Bridgeport",])
coef.lm2.b <- summary(lm2.b)$coefficients[2,1]

lm2.d = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "DETROIT",])
coef.lm2.d <- summary(lm2.d)$coefficients[2,1]
lm2.m = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "MINNEAPOLIS",])
coef.lm2.m <- summary(lm2.m)$coefficients[2,1]
lm2.r = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "Raleigh",])
coef.lm2.r <- summary(lm2.r)$coefficients[2,1]
lm2.s = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "ST PAUL",])
coef.lm2.s <- summary(lm2.s)$coefficients[2,1]
lm2.all = lm(voted01 ~ treatmen+cityturf,data=data)
coef.lm2.all <- summary(lm2.all)$coefficients[2,1]

## My analyses
# I use the underscore _my to label the coefficients which would be obtained in my analysis

lm2.b = lm(voted01 ~ treatmen,data=data[data$city == "Bridgeport",])
coef.lm2.b_my <- summary(lm2.b)$coefficients[2,1]

lm2.d = lm(voted01 ~ treatmen,data=data[data$city == "DETROIT",])
coef.lm2.d_my <- summary(lm2.d)$coefficients[2,1]
lm2.m = lm(voted01 ~ treatmen,data=data[data$city == "MINNEAPOLIS",])
coef.lm2.m_my <- summary(lm2.m)$coefficients[2,1]
lm2.r = lm(voted01 ~ treatmen,data=data[data$city == "Raleigh",])
coef.lm2.r_my <- summary(lm2.r)$coefficients[2,1]
lm2.s = lm(voted01 ~ treatmen,data=data[data$city == "ST PAUL",])
coef.lm2.s_my <- summary(lm2.s)$coefficients[2,1]
lm2.all = lm(voted01 ~ treatmen,data=data)
coef.lm2.all_my <- summary(lm2.all)$coefficients[2,1]

### Difference ### 

# Prepare data.frame
coefDiff <- data.frame(
  location = c(
    'All', 
    'Bridgeport', 
    'Detroit',
    'Minneapolis', 
    'Raleigh', 
    'St Paul'
  ), 
  ITT_original = rep(NA, 6),
  ITT_my = rep(NA, 6), 
  ITT_abs_difference = rep(NA, 6)
)

# Add original coefficients
ITT_original <- grep('coef\\.lm2\\.[^my]{,3}$', ls(), value = T)
ITT_my <- grep('coef\\.lm2\\..{0,3}\\_', ls(), value = T)

coefDiff$ITT_original <- sapply(ITT_original, get) * 100 
coefDiff$ITT_my <- sapply(ITT_my, get) * 100 
# Abs diff
coefDiff$ITT_abs_difference <- abs(coefDiff$ITT_original - coefDiff$ITT_my)

# Save
kable(coefDiff, format = 'html') %>% cat(file = 'real_data_analysis/Analysis_ITT_differences_table.html')
