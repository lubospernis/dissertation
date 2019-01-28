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
##################################################