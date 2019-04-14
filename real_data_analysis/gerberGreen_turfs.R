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
summary(lm2.b)$coefficients[2,]
lm2.c = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "COLUMBUS",])
summary(lm2.c)$coefficients[2,]
lm2.d = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "DETROIT",])
summary(lm2.d)$coefficients[2,]
lm2.m = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "MINNEAPOLIS",])
summary(lm2.m)$coefficients[2,]
lm2.r = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "Raleigh",])
summary(lm2.r)$coefficients[2,]
lm2.s = lm(voted01 ~ treatmen+cityturf,data=data[data$city == "ST PAUL",])
summary(lm2.s)$coefficients[2,]
lm2.all = lm(voted01 ~ treatmen+cityturf,data=data)
summary(lm2.all)$coefficients[2,]