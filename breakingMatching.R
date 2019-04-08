tree <- causalTree(y~ x1 + x2 + x3 + x4 + x5 + x6, data = d0, treatment = d0$t,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0.02, minsize = 20, propensity = 0.5)

library(rpart.plot)
rpart.plot(tree)

# check whether individual tree is good -> are not the interactions found spurious?
mod1 <- lm(data = d0, y ~ t + t * x1 + t * x2 + t* x3 + t* x4 + t*x5 + t*x6)
summary(mod1)

