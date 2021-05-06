
# Installing the required Packages
#install.packages("stringr", dependencies = TRUE)
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)

# Loading the required Libraries
library(tidyverse)
library(igraph)
library(statnet)
library(NetIndices)
library(dplyr)

# Setting up the path to the Dataset Directory
setwd("C:/SNA_Individual Assignment_Prineet")

# Loading the Datasets
union_characters <- readRDS(file = "nodes_table.rds")
union_edges <- readRDS(file = "edges_table.rds")

# Exploring the Nodes Table
str(union_characters)
head(union_characters)

# Exploring the Edges Table
str(union_edges)
head(union_edges)

# Loading the Data as Network
network_graph <- graph_from_data_frame(union_edges, directed = TRUE, vertices = union_characters)
network_graph

# Plotting the Network
color_vertices <- union_characters %>%   # Summarizing the node colors
  group_by(house, color) %>%
  summarise(n = n()) %>%
  filter(!is.na(color))

colors_edges <- union_edges %>%          # Summarizing the edge colors
  group_by(type, color) %>%
  summarise(n = n()) %>%
  filter(!is.na(color))

layout <- layout_with_fr(network_graph)

plot(network_graph,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(network_graph)$name),
     vertex.shape = V(network_graph)$shape,
     vertex.color = V(network_graph)$color, 
     vertex.size = (V(network_graph)$popularity + 1) * 5, 
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.1,
     edge.arrow.size = 0.1,
     edge.color = E(network_graph)$color,
     edge.lty = E(network_graph)$lty)
legend("topleft", legend = c(NA, "Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 15,
       col = c(NA, NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 1, cex = 0.5, bty = "n", ncol = 1,
       title = "") 

# Considering the Network as Undirected (to account for parent/child relationships as being mutual)
network_total <- as.undirected(network_graph, mode = "collapse")

# Checking Centrality for the Complete Network
centr_degree(network_total, mode = "total")$centralization

centr_clo(network_total, mode = "total")$centralization

centr_eigen(network_total, directed = FALSE)$centralization

################################
#### Node Degree Centrality ####
################################

network_total_degree <- igraph::degree(network_total, mode = "total")

network_total_degree_std <- network_total_degree / (vcount(network_total) - 1)  # Standardized by number of nodes

node_degree <- data.frame(degree = network_total_degree,degree_std = network_total_degree_std) %>% tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_degree, by = c("name" = "rowname"))

node_degree %>% arrange(-degree) %>% .[1:10, ]

##############################
#### Closeness Centrality ####
##############################

closeness <- igraph::closeness(network_total, mode = "total")

closeness_std <- closeness / (vcount(network_total) - 1)  # Standardized by number of nodes

node_closeness <- data.frame(closeness = closeness,closeness_std = closeness_std) %>% tibble::rownames_to_column()

union_characters <- left_join(union_characters, node_closeness, by = c("name" = "rowname"))

node_closeness %>% arrange(-closeness) %>% .[1:10, ]

###############################
#### Betweeness centrality ####
###############################

betweenness <- igraph::betweenness(network_total, directed = FALSE)

betweenness_std <- betweenness / ((vcount(network_total) - 1) * (vcount(network_total) - 2) / 2)   # Standardized by number of node pairs

# Node Betweenness
node_betweenness <- data.frame(betweenness = betweenness,betweenness_std = betweenness_std) %>% tibble::rownames_to_column() 

union_characters <- left_join(union_characters, node_betweenness, by = c("name" = "rowname"))

node_betweenness %>%arrange(-betweenness) %>% .[1:10, ]


# Edge Betweenness 
edge_betweenness <- igraph::edge_betweenness(network_total, directed = FALSE)

data.frame(edge = attr(E(network_total), "vnames"),betweenness = edge_betweenness) %>% tibble::rownames_to_column() %>% arrange(-betweenness) %>% .[1:10, ]


#Plotting node betweenness as vertex.size and edge betweenness as edge.width

plot(network_total,
       layout = layout,
       vertex.label = gsub(" ", "\n", V(network_total)$name),
       vertex.shape = V(network_total)$shape,
       vertex.color = V(network_total)$color, 
       vertex.size = betweenness * 0.001, 
       vertex.frame.color = "gray", 
       vertex.label.color = "black", 
       vertex.label.cex = 0.2,
       edge.width = edge_betweenness * 0.001,
       edge.arrow.size = 0.2,
       edge.color = E(network_total)$color,
       edge.lty = E(network_total)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 0.5, cex = 0.5, bty = "n", ncol = 1)


##################
#### Diameter ####
##################

diameter(network_total, directed = FALSE)   #We see that the longest path connects 21 nodes.

union_graph_undir_diameter <- network_total

node_diameter <- get.diameter(union_graph_undir_diameter,  directed = FALSE)

V(union_graph_undir_diameter)$color <- scales::alpha(V(union_graph_undir_diameter)$color, alpha = 0.5)
V(union_graph_undir_diameter)$size <- 2

V(union_graph_undir_diameter)[node_diameter]$color <- "red"
V(union_graph_undir_diameter)[node_diameter]$size <- 5

E(union_graph_undir_diameter)$color <- "grey"
E(union_graph_undir_diameter)$width <- 1

E(union_graph_undir_diameter, path = node_diameter)$color <- "red"
E(union_graph_undir_diameter, path = node_diameter)$width <- 5

plot(union_graph_undir_diameter,
     layout = layout,
     vertex.label = gsub(" ", "\n", V(union_graph_undir_diameter)$name),
     vertex.shape = V(union_graph_undir_diameter)$shape,
     vertex.frame.color = "gray", 
     vertex.label.color = "black", 
     vertex.label.cex = 0.2,
     edge.arrow.size = 0.2,
     edge.lty = E(union_graph_undir_diameter)$lty)
legend("topleft", legend = c("Node color:", as.character(color_vertices$house), NA, "Edge color:", as.character(colors_edges$type)), pch = 19,
       col = c(NA, color_vertices$color, NA, NA, colors_edges$color), pt.cex = 0.5, cex = 0.5, bty = "n", ncol = 1)


#############################
#### PageRank Centrality ####
#############################

page_rank <- page.rank(network_total, directed = FALSE)

page_rank_centrality <- data.frame(name = names(page_rank$vector), page_rank = page_rank$vector) %>% mutate(name = as.character(name))

union_characters <- left_join(union_characters, page_rank_centrality, by = "name")

page_rank_centrality %>% arrange(-page_rank) %>% .[1:10, ]

#################################
##### Eigenvector Centrality ####
#################################

adjacency <- as.matrix(as_adjacency_matrix(network_total))

# Degree diagonal matrix
degree_diag <- diag(1 / igraph::degree(network_total))

# PageRank matrix
pagerank <- adjacency %*% degree_diag

eigenvalues <- eigen(pagerank)

eigenvector <- data.frame(name = rownames(pagerank),eigenvector = as.numeric(eigenvalues$vectors[, which.max(eigenvalues$values)]))

union_characters <- left_join(union_characters, eigenvector, by = "name")

eigenvector %>% arrange(eigenvector) %>% .[1:10, ]

# Finding the eigenvector centrality scores
eigen_centrality <- igraph::eigen_centrality(network_total, directed = FALSE)

eigen_centrality <- data.frame(name = names(eigen_centrality$vector),eigen_centrality = eigen_centrality$vector) %>% mutate(name = as.character(name))

union_characters <- left_join(union_characters, eigen_centrality, eigenvector, by = "name")

eigen_centrality %>% arrange(-eigen_centrality) %>% .[1:10, ]

############################################
#### Summarizing all Centrality Results ####
############################################

union_characters %>%
  filter(!is.na(house2)) %>%
  dplyr::select(-contains("_std")) %>%
  gather(x, y, degree:eigen_centrality) %>%
  ggplot(aes(x = name, y = y, color = house2)) +
  geom_point(size = 1) +
  facet_grid(x ~ house2, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust= 1))

