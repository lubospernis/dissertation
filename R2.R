library(FindIt)

lalonde <- FindIt::LaLonde

model <- FindIt(model.treat = re78 ~ treat)

F1  <- FindIt(model.treat= outcome ~ treat,
             model.main= ~ age+educ+black+hisp+white+marr+nodegr+log.re75+u75,
             model.int = , 
             data = LaLonde,type="binary",
             treat.type="single")

pred <- predict(F1)

plot(pred)
summary(F1)
