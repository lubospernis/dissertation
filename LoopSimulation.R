# Treatment
t1 <- treatment_factory$new()
t1$effectLinear <- '4 + 10* -x1-'


S0 <- location_factory$new()
S0$create_sample(50, mean = 7, name = 's0')
S0$createY0()
S0$createY1()
S0$assignTreatment()
S0$ate
S1 <- location_factory$new()
S1$create_sample(50, mean = 1, name = 's1')
S1$createY0()
S1$createY1()
S1$assignTreatment()
S1$ate

predictedErrorsMa <- numeric()
covs <- paste0('x', 2:40)
used <- 'x1'

predictedErrorsFo <- numeric()
sampleCrit <- 1
for (i in covs) {
  if ('x4' < 4) {
    sampleCrit <- 3
  }

  
  S0$create_covariate(i)
  S1$create_covariate(i)
  cM <- causalMatch(S1, S0, c(used))
  pred.err <- attr(cM, 'prediction.error')
  predictedErrorsMa <- c(predictedErrorsMa, pred.err)
  
  
  cf <- causalForest(formula = as.formula(paste0('y ~ ', paste0(used, collapse = '+'))), 
                     data=S0$dfObserved, treatment=S0$dfObserved$t, 
                     split.Rule="CT", split.Honest=T,  split.Bucket=F, bucketNum = 5,
                     bucketMax = 100, cv.option="CT", cv.Honest=T, minsize = 2L, 
                     split.alpha = 0.5, cv.alpha = 0.5,
                     sample.size.total = floor(nrow(S0$dfObserved) / 2), sample.size.train.frac = .5,
                     mtry = ceiling(ncol(S0$dfObserved)/3), nodesize = 3, num.trees= 5,ncolx=length(used),ncov_sample=sampleCrit) 
  
  predictioncf <- predict(cf, S1$dfTarget)
  # pred error
  predE <- (mean(predictioncf) - S1$ate) ^2
  
  predictedErrorsFo <- c(predictedErrorsFo, predE)
  
  used <- c(used, i)
}

plot(predictedErrorsMa, type = 'o', ylim = c(0, 500))
lines(predictedErrorsFo, type = 'o', col = 'blue')
