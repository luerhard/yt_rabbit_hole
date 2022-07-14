library('dplyr')
library('igraph')
library('textclean')
library('DescTools')


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
    components = components$no,
    size = components$csize[which.max(components$csize)],
    isolates = sum(components$csize[components$csize==1]),
    avg_degree_clean = mean(degree(g)),
    viewcount_clean = sum(V(g)$view_count),
    gini_clean = Gini(V(g)$view_count)
  )
  
  # selecting largest connected component
  g <- induced_subgraph(g, vids = V(g)[components$membership %in% which.max(components$csize)])
  
  ## COMMUNITY DETECTION
  cl <- cluster_louvain(as.undirected(g))
  V(g)$cluster <- cl$membership

  MD2 <- data.frame(
    no_clusters = length(unique(cl$membership)),
    modularity = cl$modularity[1],
    avg_degree = mean(degree(g)),
    viewcount = sum(V(g)$view_count),
    gini = Gini(V(g)$view_count)
  )

  return(cbind(MD1,MD2))  
}

clean_titles <- function(g){
  V(g)$title <- replace_url(V(g)$title)
  V(g)$title <- replace_white(V(g)$title)
  V(g)$title <- gsub("&.*;", "", V(g)$title) # removing emoji
  #V(g)$title <- replace_emoticon(V(g)$title) # creates 'explained' == 'e tongue sticking out lained'
  return(g)
}

clean_descriptions <- function(g){
  # TO DO
}

add_sentiment <- function(g,i){
  perspective_data <- read.csv(paste0("../data/interim/perspective_data/",substr(i,1,nchar(i)-4),".csv"))
  title_sentiments <- read.csv(paste0("../data/interim/title_sentiments/",substr(i,1,nchar(i)-4),".csv"))
  colnames(perspective_data)[colnames(perspective_data)=="video_id"] <- "id"
  colnames(title_sentiments)[colnames(title_sentiments)=="video_id"] <- "id"
  
  network_data <- igraph::as_data_frame(g, 'both')
  
  network_data$vertices <- merge(network_data$vertices, perspective_data, by="id", all.x = TRUE)
  network_data$vertices <- merge(network_data$vertices, title_sentiments, by="id", all.x = TRUE)
  
  g <- graph_from_data_frame(network_data$edges[network_data$edges$from %in% network_data$vertices$id | network_data$edges$from %in% network_data$vertices$id,],
                             directed = T,
                             vertices = network_data$vertices)
  return(g)
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
  g <- add_sentiment(g,i)
  md <- rbind(md,get_network_metadata(g, substr(i,1,nchar(i)-4)))
  g <- clean_titles(g)
  g <- community_detect_and_select(g)
  write_graph(g, file=paste0('../data/clean/networks/',i), format = "gml")
}

## SAVE METADATA

write.csv(md, file='../data/clean/metadata/metadata_networks.csv', row.names = F)


