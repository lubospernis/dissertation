create_bipartite_graph <- function(d1, d0, covs, name = 'sampleGraph', orderBy = NULL){
  # Visualise the matching between Minnesota and St Paul 
  source('functions/causalMatchFNN_ties.R')
  library(igraph)
  
  # Match
  m <- causalMatchFNNdf_run_once_ties(d1, d0, covs)
  target <- attributes(m)$indices.list$target_data
  
  m$rowid_suf <- paste0(m$rowid, '_d0')
  target$rowid_suf <- paste0(target$rowid, '_d1')
  
  edges <- data.frame(
    from = target$rowid_suf,
    to = m$rowid_suf
  )
  
  vertices_d0 <- m[!duplicated(m$rowid_suf),]
  vertices_d1 <- target
  #order by age
  if (!is.null(orderBy)) {
    vertices_d0 <- vertices_d0[order(vertices_d0[, orderBy]),]
    vertices_d1 <- target[order(target[, orderBy]), ]
  }
  

  # Create all vertices
  vertices <- rbind(vertices_d0, vertices_d1)
  vertices <- vertices[, c('rowid_suf', colnames(vertices)[which(colnames(vertices)!='rowid_suf')])]
  # Greate graph
  g3 <- graph_from_data_frame(d= edges, vertices = vertices, directed = F)
  # Change to bipart
  V(g3)$type <- bipartite_mapping(g3)$type
  # Change coords of the plotting 
  matl <- as.matrix(data.frame(
    x = c(seq(from = 1, to = 1420, length.out = nrow(vertices_d0)), seq(from = 1, to = 1420, length.out = nrow(vertices_d1))), 
    y = c(rep(0, nrow(vertices_d0)), rep(1, nrow(vertices_d1)))
  ))
  # Change size to equal the degree
  V(g3)$size <- degree(g3, v = V(g3), mode = 'total',
                       loops = TRUE, normalized = FALSE)
  
  # Change colour according to treatment 
  V(g3)$color <- ifelse(V(g3)$t == 1, 'red', 'blue')
  
  # Change shape according to whether the person participated in the last election
  V(g3)$shape <- ifelse(V(g3)$voted00 == 1, "circle", "square")
  
  # Plot
  
  png(filename = sprintf('images/%s.png', name), width = 800, height = 800)
  plot(g3, layout = matl,vertex.label = NA)
  dev.off()
  
  return('done')
}