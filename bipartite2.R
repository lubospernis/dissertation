# Visualise the matching between Minnesota and St Paul 
source('functions/causalMatchFNN_ties.R')
library(igraph)
library(scales)
library(ggplot2)
df_all <- readRDS('data/gg_clean.Rds')


#### Minnesota ####
# Subset data
df <- df_all[df_all$d == 'St Paul' | df_all$d == 'Minneapolis',]

### Covariate imbalances ###
# Note that we only match on age and voted00

# Age
ggplot(data = df, aes(age, fill = d)) + 
  geom_density(alpha = 0.3)

# Voted00
hist(df$voted00[df$d == 'St Paul'], freq = F)
hist(df$voted00[df$d == 'Minneapolis'], freq = F, add = T, col = alpha('red', 0.3))

# Scale
df$age <- rescale(
  df$age, 
  to = c(0, 1)
)

# Let D = 1 be Minneapolis and D = 0 St Paul
d1 <- df[df$d == 'Minneapolis', ]
d0 <- df[df$d == 'St Paul',]


# Match
m <- causalMatchFNNdf_run_once_ties(d1, d0, c('age', 'voted00'))
target <- attributes(m)$indices.list$target_data

m$rowid_suf <- paste0(m$rowid, '_d0')
target$rowid_suf <- paste0(target$rowid, '_d1')

edges <- data.frame(
  from = target$rowid_suf,
  to = m$rowid_suf
)

vertices_d0 <- m[!duplicated(m$rowid_suf),]
#order by age
vertices_d0 <- vertices_d0[order(vertices_d0$age),]
vertices_d1 <- target[order(target$age), ]

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


V(g3)$label <- ifelse(V(g3)$size > 5, V(g3)$y, NA)

# Plot

png(filename = 'images/graph1.png', width = 800, height = 800)
plot(g3, layout = matl)
dev.off()

# Analyze the degrees
# Do the more connected nodes have lower potential outcomes? 

# St Paul
mod1 <- lm(data = d0[d0$age < 0.25,], y ~ t + voted00)
summary(mod1)
# Minneapolis
mod2 <- lm(data = d1[d1$age < 0.25,], y ~ t + voted00)
summary(mod2)

# t is bigger for Minneapolis in this local linear.
