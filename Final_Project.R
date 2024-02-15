setwd("C:/Users/SAGAR/OneDrive/Desktop/Sagar/NCSU/MEM/DSC 595")
# Load the dataset
load("C:/Users/SAGAR/OneDrive/Desktop/Sagar/NCSU/MEM/DSC 595/kapfm.rda")
library(igraph)

kapfmm
kapfmu
par(mar=c(2,2,2,2))
plot(kapfmm,
     main = "KAPFMM Network")
plot(kapfmu,
     main = "KAPFMU Network")

V(kapfmu)$name
V(kapfmm)$name



# Categorize and assign colors based on the first letter of node names
first_letters <- substr(V(kapfmm)$name, 1, 1)
unique_letters <- unique(first_letters)
color_palette <- rainbow(length(unique_letters))  # Create a color palette
names(color_palette) <- unique_letters

# Assign colors to the nodes
V(kapfmm)$color <- color_palette[first_letters]

# Plot the graph
# Set plotting margin
par(mar = c(2,2,2,2))
plot(kapfmm, vertex.color = V(kapfmm)$color, 
     vertex.label.cex = 0.7, 
     vertex.size = 5
     )

# Add a legend
par(mar = c(2,2,2,2))
legend("topright", # position of the legend
       legend = unique_letters, # labels for the legend
       col = color_palette[unique_letters], # colors for the legend
       pch = 19, # type of point to use
       title = "First Letter", # title of the legend
       cex = 0.7) # size of the text in the legend

library(visNetwork)

V(kapfmm)$name

# Create a nodes data frame
nodes <- data.frame(id = 1:vcount(kapfmm), label = V(kapfmm)$name)

# Create an edges data frame
# Extract edges from the igraph object and map names to IDs
edges <- get.data.frame(kapfmm, what = "edges")
edges$from <- match(edges$from, nodes$label)
edges$to <- match(edges$to, nodes$label)

edges

# Create the network visualization
visNetwork(nodes, edges) %>%
  visNodes(color = "red", shape= "star") %>%
  visEdges(smooth = TRUE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


V(kapfmu)$name

# Create a nodes data frame
nodes1 <- data.frame(id = 1:vcount(kapfmu), label = V(kapfmu)$name)

# Create an edges data frame
# Extract edges from the igraph object and map names to IDs
edges1 <- get.data.frame(kapfmu, what = "edges")
edges1$from <- match(edges1$from, nodes$label)
edges1$to <- match(edges1$to, nodes1$label)

edges1

# Create the network visualization
visNetwork(nodes1, edges1) %>%
  visNodes(color = "red", shape= "star") %>%
  visEdges(smooth = TRUE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


##Compute centrality scores of KAPFMM and KAPFMU:
#1.Degree centrality
deg <- degree(kapfmm)
deg

#2. Closeness centrality
close <- closeness(kapfmm)
close
#3. Betweenness centrality
bet <- betweenness(kapfmm)
bet
#4. Eigenvector centrality
eigens_vec<- eigen_centrality(kapfmm, scale = FALSE)
eigens_vec$vector

# Centralization with isolates
centr_eigen(kapfmm, directed=FALSE)$centralization


##Compute centrality scores of KAPFMU
#1.Degree centrality
deg <- degree(kapfmu)
deg

#2. Closeness centrality
close <- closeness(kapfmu)
close
#3. Betweenness centrality
bet <- betweenness(kapfmu)
bet

#4. Eigenvector centrality
eigens_vecmu<- eigen_centrality(kapfmu, scale = FALSE)
eigens_vecmu$vector

# Centralization 
centr_eigen(kapfmu, directed=FALSE)$centralization




# Remove isolates from kapfmm
kapfmm_without_isolates <- delete.vertices(kapfmm, V(kapfmm)[degree(kapfmm) == 0])

# Centralization without isolates
centr_eigen(kapfmm_without_isolates, directed=FALSE)$centralization

# Categorize and assign colors based on the first letter of node names
first_letters <- substr(V(kapfmm_without_isolates)$name, 1, 1)
unique_letters <- unique(first_letters)
color_palette <- rainbow(length(unique_letters))  # Create a color palette
names(color_palette) <- unique_letters

# Assign colors to the nodes
V(kapfmm_without_isolates)$color <- color_palette[first_letters]

# Plot the graph
# Set plotting margin
par(mar = c(2,2,2,2))
plot(kapfmm_without_isolates, vertex.color = V(kapfmm_without_isolates)$color, 
     vertex.label.cex = 0.7, 
     vertex.size = 5
)

# Add a legend
par(mar = c(2,2,2,2))
legend("topright", # position of the legend
       legend = unique_letters, # labels for the legend
       col = color_palette[unique_letters], # colors for the legend
       pch = 19, # type of point to use
       title = "First Letter", # title of the legend
       cex = 0.7) # size of the text in the legend


#Finding Largest Cliques of KAPFMM:
L <-largest_cliques(kapfmm_without_isolates)
L
#Data Frame of Largest Cliques of KAPFMM:
cliques <- as.data.frame(largest_cliques(kapfmm_without_isolates))
cliques

#Computing Coreness of KAPFMM:
coreness <- graph.coreness(kapfmm_without_isolates)
coreness


#Finding Largest Cliques of KAPFMU:
L1 <-largest_cliques(kapfmu)
L1
#Data Frame of Largest Cliques of KAPFMU:
cliques1 <- as.data.frame(largest_cliques(kapfmu))
cliques1

#Computing Coreness of KAPFMU:
coreness1 <- graph.coreness(kapfmu)
coreness1




# Create a numerical membership vector based on a criterion (e.g., first letter of names)
first_letters <- substr(V(kapfmm_without_isolates)$name, 1, 1)
unique_letters <- unique(first_letters)
letter_to_group <- match(first_letters, unique_letters)

# Calculate modularity of KAPFMM dataset
mod <- modularity(kapfmm_without_isolates, letter_to_group)
mod



#Calculate assortativity coefficient for 'namer' using KAPFMM
assortativity_nominal(
  kapfmm_without_isolates, 
  as.integer(as.factor(V(kapfmm_without_isolates)$name)))


# Comparision using centralisation, modularity and assortivity with KAPFMU

# Centralization without isolates
centr_eigen(kapfmu, directed=FALSE)$centralization

assortativity_nominal(
  kapfmu, 
  as.integer(as.factor(V(kapfmu)$name)))


# Create a numerical membership vector based on a criterion (e.g., first letter of names)
first_letters <- substr(V(kapfmu)$name, 1, 1)
unique_letters <- unique(first_letters)
letter_to_group <- match(first_letters, unique_letters)

# Calculate modularity
mod <- modularity(kapfmu, letter_to_group)
mod


# Ensure the necessary libraries are loaded
library(igraph)
library(ggraph)

#Performs the Louvain community detection on the kapfmm network understand:
louvain_comm <- cluster_louvain(kapfmm_without_isolates)
louvain_comm

# Get community memberships from the Louvain community detection result
communities <- membership(louvain_comm)
communities

# Assign these memberships to the vertices of your graph
V(kapfmm_without_isolates)$louvain_community <- communities
V(kapfmm_without_isolates)$louvain_community

vertex.attributes(kapfmm_without_isolates)

# Plot the network
set.seed(123)

ggraph(kapfmm_without_isolates, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(color = as.factor(louvain_community)), size = 5) +
  geom_node_text(aes(label = name), vjust = 1.5, size = 3) +  
  labs(title = "KAPFMM Graph") +
  theme_minimal()


#Performs the Louvain community detection on the kapfmu network understand:
louvain_comm1 <- cluster_louvain(kapfmu)
louvain_comm1

# Get community memberships from the Louvain community detection result
communities1 <- membership(louvain_comm1)
communities1

# Assign these memberships to the vertices of your graph
V(kapfmu)$louvain_community <- communities1
V(kapfmu)$louvain_community

vertex.attributes(kapfmu)

# Plot the network
set.seed(123)

ggraph(kapfmu, layout = "fr") +
  geom_edge_link(color = "grey", alpha = 0.7) +
  geom_node_point(aes(color = as.factor(louvain_community)), size = 5) +
  geom_node_text(aes(label = name), vjust = 1.5, size = 3) +  
  labs(title = "KAPFMU Graph") +
  theme_minimal()






# Load necessary libraries
library(igraph)
library(blockmodeling)

# Set a seed value to ensure reproducibility
set.seed(54321)

# Get adjacency matrix from the igraph object 'kapfmm'
mat <- as.matrix(get.adjacency(kapfmm_without_isolates))

# Estimate blockmodel partitions with k = 6 using the optRandomParC command
class6 <- optRandomParC(M=mat, k=6, rep=10, approach="ss", blocks="com")
class6

# Retrieve the best partition from the blockmodeling result
best_partition <- class6$best$best1$clu
best_partition

# Assign the block designations to the igraph object 'kapfmm_without_isolates'
V(kapfmm_without_isolates)$block <- best_partition
V(kapfmm_without_isolates)$block

# Check if the number of blocks exceeds the default R color palette
if (max(best_partition) > length(colors())) {
  cat("Warning: Number of blocks exceeds the number of default colors in R.\n")
}

# Plot the graph with vertices colored by their block designation
par(mar=c(1,1,1,1), mfrow=c(1,1))
plot(kapfmm_without_isolates,
     vertex.label=NA,  # Hide vertex labels for a cleaner plot
     vertex.size=5,  # Set vertex size
     edge.arrow.size=.5,  # Set edge arrow size
     vertex.color=V(kapfmm_without_isolates)$block,  # Color vertices by block
     main="KAPFMM Blockmodel ")  # Title for the plot


mat1 <- as.matrix(get.adjacency(kapfmu))

# Estimate blockmodel partitions with k = 6 using the optRandomParC command
class6 <- optRandomParC(M=mat1, k=6, rep=10, approach="ss", blocks="com")
class6

# Retrieve the best partition from the blockmodeling result
best_partition1 <- class6$best$best1$clu
best_partition1

# Assign the block designations to the igraph object 'kapfmu'
V(kapfmu)$block <- best_partition1
V(kapfmu)$block

# Check if the number of blocks exceeds the default R color palette
if (max(best_partition) > length(colors())) {
  cat("Warning: Number of blocks exceeds the number of default colors in R.\n")
}

# Plot the graph with vertices colored by their block designation
par(mar=c(1,1,1,1), mfrow=c(1,1))
plot(kapfmu,
     vertex.label=NA,  # Hide vertex labels for a cleaner plot
     vertex.size=5,  # Set vertex size
     edge.arrow.size=.5,  # Set edge arrow size
     vertex.color=V(kapfmu)$block,  # Color vertices by block
     main="KAPFMU Blockmodel ")  # Title for the plot

