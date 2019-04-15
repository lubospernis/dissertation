# Visualise matching as a bipartite graph
library(igraph)

# Match
m <- causalMatchFNNdf_run_once_ties(d1, d0, c('age', 'voted00'))
rownames(m)
d0[rownames(d0) == 18841,]


target <- attributes(m)$indices.list$target_data_t
initial <- attributes(m)$indices.list$initial_data_t
indices <- attributes(m)$indices.list$index_initial


bipart <- data.frame(
  d1_row = as.numeric(rownames(target)), 
  d0_row = indices
)

bipart$ageOrder <- target[rownames(target) == rownames(target),'age']
bipart2 <- bipart[order(bipart$ageOrder),]

# Create a graph
g <- graph.data.frame(bipart2, directed=FALSE)
# Map as bipartite
V(g)$type <- bipartite_mapping(g)$type
# Adjust size to the degree
V(g)$size <- degree(g, v = V(g), mode = 'total',
                    loops = TRUE, normalized = FALSE)

# voted in d = 0 
V(g)$voted <- NA
V(g)$voted[V(g)[V(g)$type == T]] <- initial[as.numeric(names(V(g)[V(g)$type == T])), 'voted00']
V(g)$voted[V(g)[V(g)$type == F]] <- target[rownames(target) == names(V(g)[V(g)$type == F]), 'voted00']
# Shape according to voting in 00 election
V(g)$shape <- ifelse(V(g)$voted == 1, "circle", "square")


# age
V(g)$age <- NA
V(g)$age[V(g)[V(g)$type == T]] <- initial[as.numeric(names(V(g)[V(g)$type == T])), 'age']
V(g)$age[V(g)[V(g)$type == F]] <- target[rownames(target) == names(V(g)[V(g)$type == F]), 'age']

# Color
V(g)$color <- rescale(V(g)$age, c(0, 100))
V(g)$color <- round(V(g)$color)
g$palette <- heat.colors(100)   

# Edges color
E(g)$color <- "lightgray"




# Layout according to age

plot(g, layout=layout.bipartite, vertex.label = NA)

