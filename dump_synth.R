library(Synth)
data("basque")

analysis <- basque[85:89, 1:4]

# causal tree

t <- c(rep(1, 5), rep(0, 5))
x <- c(rep(1, 3), 0.8, 0.7, rep(0, 2), 0.1, 0.2, 0.01)
y <- c(rep(1, 3), 0.6,0.2, ep(0, 3), 0.1, 0.5)

library(causalTree)

df <- data.frame(t, x, y)

tree <- causalTree::causalTree(y ~ x, data = df, treatment = t, cv.option = "no")
plot(tree)

data('simulation.1')
print(simulation.1)
