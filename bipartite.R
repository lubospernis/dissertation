# Visualise matching as a bipartite graph

library(igraph)

m <- causalMatchFNNdf_run_once_ties(d1, d0, c('age', 'voted00'))

# goes from here - every individual from these
rownames(attributes(m)$indices.list$target_t)

# matched to - more links can come here 
length(unique((attributes(m)$indices.list$initial_t)))

bipart <- data.frame(
  d1_row = as.numeric(rownames(attributes(m)$indices.list$target_t)), 
  d0_row = attributes(m)$indices.list$initial_t
)

g <- graph.data.frame(bipart, directed=FALSE)
V(g)$type <- bipartite_mapping(g)$type


V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")

plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
