sampleExp <- data.frame(
  x1 = c(rep(19, 50), rep(21, 50)), 
  gdp = 10
)

set.seed(123)
t <- sample(1:100)

sampleExp <- sampleExp[order(t), ,drop = F]
sampleExp$t <- c(rep(1, 50), rep(0, 50))

cor(sampleExp$t, sampleExp$x1)

library(dplyr)
sampleExp <- sampleExp %>% mutate(
  y = case_when(
    x1 < 20 & t == 1 ~ 200, 
    x1 < 20 & t == 0 ~ 100, 
    x1 > 20 & t == 1 ~ 400, 
    x1 > 20 & t == 0 ~ 200
  )
)

library(causalTree)

buidlTree <- causalTree(y~ x1 , data = sampleExp, treatment = sampleExp$t,
                       split.Rule = "fit", cv.option = "fit", split.Honest = T, cv.Honest = T, split.Bucket = T, bucketNum = 5,
                       bucketMax = 200, xval = 10, 
                       cp = 0, minsize = 20, propensity = 0.5)
rpart.plot(buidlTree)

vals <- predict(buidlTree, sampleExp)

sampleExp$tau <- vals

# Step 2
mod1 <- lm(data = sampleExp, tau ~ x1)
summary(mod1)
coef(mod1)

tree <- rpart(tau ~ x1, data = sampleExp)
rpart.plot(tree)

sampleExp

# Repeat the same thing for another location to get fitted val

sampleExp0 <- data.frame(
  x1 = c(rep(19, 50), rep(21, 50)), 
  gdp = 20
)

set.seed(123)
t <- sample(1:100)

sampleExp0 <- sampleExp0[order(t), ,drop = F]
sampleExp0$t <- c(rep(1, 50), rep(0, 50))

cor(sampleExp0$t, sampleExp0$x1)

library(dplyr)
sampleExp0 <- sampleExp0 %>% mutate(
  y = case_when(
    x1 < 20 & t == 1 ~ 300, 
    x1 < 20 & t == 0 ~ 100, 
    x1 > 20 & t == 1 ~ 500, 
    x1 > 20 & t == 0 ~ 200
  )
)

library(causalTree)

buidlTree0 <- causalTree(y~ x1 , data = sampleExp0, treatment = sampleExp0$t,
                        split.Rule = "fit", cv.option = "fit", split.Honest = T, cv.Honest = T, split.Bucket = T, bucketNum = 5,
                        bucketMax = 200, xval = 10, 
                        cp = 0, minsize = 20, propensity = 0.5)
rpart.plot(buidlTree0)

vals0 <- predict(buidlTree0, sampleExp0)

sampleExp0$tau <- vals0

# Step 2
mod10 <- lm(data = sampleExp0, tau ~ x1)
summary(mod10)
coef(mod10)

tree0 <- rpart(tau ~ x1, data = sampleExp0)
rpart.plot(tree0)

sampleExp0

# bind them together

together <- rbind(sampleExp, sampleExp0)


runtree <- rpart(tau ~ x1 + gdp, data = together)
rpart.plot(runtree)
together
runtree
summary(runtree)
