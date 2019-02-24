#install.packages("MatchIt")
library(MatchIt)
ldata <- MatchIt::lalonde


#install.packages("devtools")
library(devtools) 
install_github("susanathey/causalTree")
library(causalTree)

tree <- causalTree(re78 ~ age + educ + black + hispan + married, 
                   data = ldata, treatment = ldata$treat,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)

rpart.plot(tree)

tree2 <- causalTree(moderacy ~ age + literate + urban, data = h, treatment = h$success, split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                    xval = 5, cp = 0, minsize = 20, propensity = 0.5)

rpart.plot(tree2)
