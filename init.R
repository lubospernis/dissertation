##################################################
## Section: Load packages
library(foreign)

# This is a special stata file
install.packages("readstata13")

library(readstata13)
##################################################

##################################################
## Section: Load in the dta file
imm <- read.dta13('data/imm.bjpols.dta')

str(imm)

aggregate(imm[, c("age", "gender")], list(
  Region = imm$country
), mean)
##################################################

##################################################
## Section: mexico
all <- read.table("data/ALL_Mexico.tab", header = T)
colnames(all)
##################################################

##################################################
## Section: china
china <- read.dta13('data/china/data.dta')
##################################################