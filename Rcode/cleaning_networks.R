library('dplyr')
library('igraph')

## FUNCTIONS ----

clean_network <- function(g){
  ## NODE SELECTION
  # saving indegree count
  V(g)$indegree <- degree(g, V(g), mode="in")
  # delete vertices that were not in the original dataset
  g <- delete_vertices(g, V(g)[V(g)$step == 1])
  # # delete vertices with low viewcount
  # g <- delete_vertices(g, V(g)[V(g)$view_count < 100])
  # # delete vertices with low indegree
  # g <- delete_vertices(g, V(g)[V(g)$indegree > 0])
  
  ## EDGE SELECTION
  # selecting edges
  g <- delete_edges(g, E(g)[E(g)$samechannel == FALSE])
  g <- delete_edges(g, E(g)[E(g)$rank > 20])
  # g <- delete_edges(g, E(g)[E(g)$rank > 25])
}

get_network_metadata <- function(g, networkName=NA){
  ## COMPONENT SELECTION
  # identifying components
  components <- components(g)
  
  MD1 <- data.frame(
    name = networkName,
    size_clean = length(V(g)),
    num_components = components$no,
    size_largest_component = components$csize[which.max(components$csize)],
    isolates = sum(components$csize[components$csize==1])
  )
  
  # selecting largest connected component
  g <- induced_subgraph(g, vids = V(g)[components$membership %in% which.max(components$csize)])
  
  ## COMMUNITY DETECTION
  cl <- cluster_louvain(as.undirected(g))
  V(g)$cluster <- cl$membership

  MD2 <- data.frame(
    no_clusters = length(unique(cl$membership)),
    modularity = cl$modularity[1]
  )

  return(cbind(MD1,MD2))  
}

community_detect_and_select <- function(g){
  ## COMPONENT SELECTION
  # identifying components
  components <- components(g)
  # selecting largest connected component
  g <- induced_subgraph(g, vids = V(g)[components$membership %in% which.max(components$csize)])
  
  ## COMMUNITY DETECTION
  cl <- cluster_louvain(as.undirected(g))
  V(g)$cluster <- cl$membership
  
  return(g)
}


## CLEANING ----

network_files <- list.files(path="../data/interim/networks", pattern="*.gml", full.names=FALSE, recursive=FALSE)

md <- data.frame()

for(i in network_files) {
  g <- read_graph(paste0("../data/interim/networks/",i), format = "gml")
  g <- clean_network(g)
  md <- rbind(md,get_network_metadata(g, substr(i,1,nchar(i)-4)))
  g <- community_detect_and_select(g)
  write_graph(g, file=paste0('../data/clean/networks/',i))
}

## SAVE METADATA

write.csv(md, file='../data/clean/metadata/metadata_networks.csv', row.names = F)
