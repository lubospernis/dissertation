# or rather the treatment effect for each of the people I would say  - 
# because I do not really care about estimating Y0

# you can have potential outcomes under control the same - need to be the same conditioning
# on location and covariates
# So the first part of the problem that D is orthogonal to Y0 is solved by design because
# This is going to be the case with every unit

# what cannot I have

# individual 1 - age 10, educ 3 => y0 = 10
# individual 2 - age 10, eudc 3 => y0 = 12

# Here if I adjusted all of the covariates, I could not retrieve the true ATE

# what can I have

# individual 1 - age 10, educ 3 => y0 = 10
# individual 2 - age 10, educ 2 => y0 = 10

# here if adjusted all of the covariates, this would work

# this is just saying that the second thing does not impact potential outcomes much or at all
# and is orthogonal in the equation



df <- data.frame(n = rep(3, 10), k = seq(0.1, 1, by = 0.1))

out <- apply(df, 1, function(x){
  out <- numeric()
  for (i in 1:length(x)) {
    if (i == 1) {
      out <- x[i]
    } else {
      out <- out * x[i]
    }
  }
  return(abs(out))
})


