# generate sample of videos to hand-code

library('igraph')

conspiracy_topics <- c(
  "plandemic","5g_covid","is_earth_flat","pizzagate",
  "adrenochrome","qanon","chemtrails","great_replacement_theory",
  "9-11_building_7","death_elvis_presley"
)

## FUNCTIONS ----

selective_sampling <- function(g,nameNetwork=NA){
  mostviewed <- V(g)$label[order(V(g)$viewcount,decreasing=T)][1:ceiling(.1*length(V(g)))]
  highestind <- V(g)$label[order(V(g)$indegree,decreasing=T)][1:ceiling(.1*length(V(g)))]
  vidsselect <- unique(c(mostviewed,highestind))
  randomvids <- sample(setdiff(V(g)$label, vidsselect),
                       size=ceiling((.3 - (length(vidsselect) / length(V(g)))) * length(V(g))))
  totalsampl <- sample(c(vidsselect,randomvids))
  
  return(data.frame(name=nameNetwork,video_id=totalsampl))
} 

## GENERATE CSVs WITH LABEL SAMPLES ----

set.seed(187)

network_files <- list.files(path="../data/clean/networks", pattern="*.gml", full.names=FALSE, recursive=FALSE)

df <- data.frame()

for(i in intersect(network_files,paste0(conspiracy_topics, '.gml'))) {
  g <- read_graph(paste0("../data/clean/networks/",i), format = "gml")
  df <- rbind(df,selective_sampling(g, substr(i,1,nchar(i)-4)))
}

write.csv(df, file="../data/clean/label sample/label_ids.csv")
