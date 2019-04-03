source('synthData_factories.R')

# Situation of heterogenous treatment effects depending on the value of age

HU <- location_factory$new()
HU$create_sample(500, mean = -4, "Hungary", 123)
HU$createY0(123)
# Create new treatment
treatment2 <- treatment_factory$new()
# Specify the effect
treatment2$effectLinear <- '-y0- + 0.8*(-x1- + 1) + 2'
# Create Y1
HU$createY1(123)
# Assign treatment
HU$assignTreatment(123)
# Only work with the observed files
HU$dfObserved
HU$ate

## Testing heterogeneity
# Interaction effects - so yes
model1 <- lm(data = HU$dfObserved, y ~ t * x1)
summary(model1)

###  Now generalise - start from the Hotz paper ### 
# We want to generalise to Brazil
BR <- location_factory$new()
BR$create_sample(500, mean = 4, "Brazil", 123)
BR$createY0(123)
BR$createY1(123)
BR$assignTreatment(123)

# Compare Brazil to Hungary
par(mfrow = c(1, 2))
HU$show_sample_distribution()
BR$show_sample_distribution()
dev.off()


# Inspect the ATEs
HU$ate
BR$ate

HUAte <- lm(data = HU$df, y ~ t)
sHU <-summary(HUAte)

BRAte <- lm(data = BR$df, y ~ t)
sBR <- summary(BRAte)


# So are these two significantly different?

# So what can we do?
# 1. We can plot the effects with sd errors
library(ggplot2)

coefs <- data.frame(
  low = c(coef(HUAte)[2] - 1.96 * sHU$coefficients[2, 2], coef(BRAte)[2] - 1.96 * sBR$coefficients[2, 2]),
  es = c(coef(HUAte)[2], coef(BRAte)[2]),
  hi = c(coef(HUAte)[2] + 1.96 * sHU$coefficients[2, 2], coef(BRAte)[2] + 1.96 * sBR$coefficients[2, 2]), 
  country = c(1, 2)
)

ggplot(coefs, aes(y=country, x=es)) +
  geom_errorbarh(aes(xmin=low, xmax=hi), height=.1, col="red") +
  geom_point(aes(y=country, x=es), size=2, col="red", shape=21, fill="red")

# Check the balance in covariates
t.test(
  HU$df$x1[HU$df$t == 1], 
  HU$df$x1[HU$df$t == 0]
)

mean(HU$df$x1[HU$df$t == 1]) - mean(HU$df$x1[HU$df$t == 0]) 


cor(HU$df$x1, HU$df$t)

# Over repeated sampling
replicationx1 <- function(d, cov) {
 d$assignTreatment()
 r <- mean(HU$df[HU$df$t == 1, cov]) - mean(HU$df[HU$df$t == 0, cov]) 
 return(r)
}

out <- replicate(1000, replicationx1(HU, "x1"))
plot(density(out))
mean(out)
