# Gerber and Green data 
# https://isps.yale.edu/research/data/d017

# Load dataset
gg <- read.csv('data/GreenGerberNickerson_JP_2003.csv')

# Matching
library(Matching)
library(MatchIt)
library(caret)
?Match

# Binary locations
dummyGG <- dummyVars(~city, gg)

dummyGG
ggBin <- cbind(gg, predict(dummyGG, gg))

# Subset to St Paul and Bridgeport 
ggBinSub <- ggBin[ggBin$city == 'Bridgeport' | ggBin$city == "ST PAUL", ]

ggBinSub <- ggBinSub[-which(is.na(ggBinSub$age)), ]
# Now match
m <- Match(Tr = ggBinSub$city.Bridgeport, 
           X = cbind(ggBinSub$race,
                     ggBinSub$sex,
                     ggBinSub$age), 
           ties = FALSE)

pred <- ggBinSub[m$index.control, ]

# Now I took St Paul and predicted to Bridgeport
predATE <- mean(pred$voted01[pred$treatmen == 1]) - mean(pred$voted01[pred$treatmen == 0])
predATE

# I can try here more