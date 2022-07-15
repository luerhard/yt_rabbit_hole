# analysis meso

library('igraph')
library('ggplot2')
library('ggalluvial')

theme_set(
  theme_classic() +
    theme(text=element_text(size=12, color='black', family="Open Sans"),
          axis.text=element_text(size=12, color='black', family="Open Sans"))
)

## READ DATA ----

network_files <- list.files(path="../data/clean/networks", pattern="*.gml", full.names=FALSE, recursive=FALSE)

nodes <- data.frame()
edges <- data.frame()

for(i in network_files){
  g <- read_graph(paste0('../data/clean/networks/',i), format="gml")
  
  new_nodes <- igraph::as_data_frame(g, 'vertices')
  new_edges <- igraph::as_data_frame(g, 'edges')
  new_edges <- merge(new_edges, data.frame(from=new_nodes$id, source=new_nodes$label), by='from')
  new_edges <- merge(new_edges, data.frame(to=new_nodes$id, target=new_nodes$label), by='to')
  
  new_edges$name <- substr(i,1,nchar(i)-4)
  new_nodes$name <- substr(i,1,nchar(i)-4)
  
  nodes <- rbind(nodes, new_nodes)
  edges <- rbind(edges, new_edges)
}

conspiracy_topics <- c(
  "plandemic","5g_covid","is_earth_flat","pizzagate",
  "adrenochrome","qanon","chemtrails","great_replacement_theory",
  "9_11_building_7","death_elvis_presley")
noncontroversial_topics <- c(
  "how_to_draw","ab_workout","warrior_cats",
  "vintage_jewelry","pokemon_go","minecraft","wordle","van_life_us",
  "power_tools","urban_gardening")
news_topics <- c(
  "dominion_voting_system","roe_v_wade","critical_race_theory",
  "johnny_depp_amber_heard_trial","vaccine_mandate","derek_chauvin",
  "baby_formula","transgender","antifa","gas_prices")
science_topics <- c(
  "game_theory","filter_bubbles","nft","climate_change",
  "monkeypox_virus","nanotechnology","blockchain","machine_learning","autism",
  "tourette_syndrome")

nodes$category[nodes$name %in% conspiracy_topics] <- "conspiracy"
nodes$category[nodes$name %in% noncontroversial_topics] <- "non-controversial"
nodes$category[nodes$name %in% news_topics] <- "news"
nodes$category[nodes$name %in% science_topics] <- "science"

nodes$category <- factor(nodes$category, levels=c("non-controversial","science","news","conspiracy"))

nodes$name <- gsub("_", " ", nodes$name)


## ALLUVIAL ----

lr_connect <- merge(data.frame(source=edges$source, target=edges$target),
                    data.frame(source=nodes$label, source_lr=nodes$leftright, 
                               network=nodes$name, category=nodes$category,
                               weight=1),
                    by='source')
lr_connect <- merge(lr_connect,
                    data.frame(target=nodes$label, target_lr=nodes$leftright),
                    by='target')
lr_connect$source_lr[is.na(lr_connect$source_lr)] <- "NA"
lr_connect$target_lr[is.na(lr_connect$target_lr)] <- "NA"

lr_connect <- aggregate(weight ~ source_lr + target_lr + category, data=lr_connect, FUN=sum)


## News

df_news <- lr_connect[lr_connect$category=="news",]

edgecolors <- df_news$target_lr
edgecolors[edgecolors=="R"] <- 'grey'
edgecolors[edgecolors=="C"] <- 'red'
edgecolors[edgecolors=="L"] <- 'gold'
edgecolors[edgecolors=="NA"] <- 'blue'
edgecolors <- c(edgecolors,edgecolors)

df_news$source_lr <- factor(df_news$source_lr, levels = c("L","C","R","NA"))
df_news$target_lr <- factor(df_news$target_lr, levels = c("L","C","R","NA"))

ggplot(df_news,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12) +
  geom_stratum(width=1/12, fill='white') +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_void()
ggsave('../../plots/meso_alluvial_news.png', width=3.5, height=5, dpi=300)


## Science

df_science <- lr_connect[lr_connect$category=="science",]

edgecolors <- df_science$target_lr
edgecolors[edgecolors=="R"] <- 'grey'
edgecolors[edgecolors=="C"] <- 'red'
edgecolors[edgecolors=="L"] <- 'gold'
edgecolors[edgecolors=="NA"] <- 'blue'
edgecolors <- c(edgecolors,edgecolors)

df_science$source_lr <- factor(df_science$source_lr, levels = c("L","C","R","NA"))
df_science$target_lr <- factor(df_science$target_lr, levels = c("L","C","R","NA"))

ggplot(df_science,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12) +
  geom_stratum(width=1/12, fill='white') +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_void()
ggsave('../../plots/meso_alluvial_science.png', width=3.5, height=5, dpi=300)


## Non-controversial

df_nc <- lr_connect[lr_connect$category=="non-controversial",]

edgecolors <- df_nc$target_lr
edgecolors[edgecolors=="R"] <- 'grey'
edgecolors[edgecolors=="C"] <- 'red'
edgecolors[edgecolors=="L"] <- 'gold'
edgecolors[edgecolors=="NA"] <- 'blue'
edgecolors <- c(edgecolors,edgecolors)

df_nc$source_lr <- factor(df_nc$source_lr, levels = c("L","C","R","NA"))
df_nc$target_lr <- factor(df_nc$target_lr, levels = c("L","C","R","NA"))

ggplot(df_nc,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12) +
  geom_stratum(width=1/12, fill='white') +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_void()
ggsave('../../plots/meso_alluvial_nc.png', width=3.5, height=5, dpi=300)


## Conspiracy

df_conspiracy <- lr_connect[lr_connect$category=="conspiracy",]

edgecolors <- df_conspiracy$target_lr
edgecolors[edgecolors=="R"] <- 'grey'
edgecolors[edgecolors=="C"] <- 'red'
edgecolors[edgecolors=="L"] <- 'gold'
edgecolors[edgecolors=="NA"] <- 'blue'
edgecolors <- c(edgecolors,edgecolors)

df_conspiracy$source_lr <- factor(df_conspiracy$source_lr, levels = c("L","C","R","NA"))
df_conspiracy$target_lr <- factor(df_conspiracy$target_lr, levels = c("L","C","R","NA"))

ggplot(df_conspiracy,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12) +
  geom_stratum(width=1/12, fill='white') +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  theme_void()
ggsave('../../plots/meso_alluvial_conspiracy.png', width=3.5, height=5, dpi=300)











ggplot(lr_connect[lr_connect$network=="5g covid",],
       aes(y = weight, axis1 = source_lr, axis2 = target_lr)) +
  geom_alluvium(fill = '#BED1DB',#edgecolors, 
                width=1/12) + #aes(fill = Admit), width = 1/12) +
  geom_stratum(fill = '#BED1DB',#nodecolors, 
               width = 1/12, lwd=.25, color = "#fafafa") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), label=c(rep('',23),c(5:1),rep('',22),c(5:1)), fill=NA, color='white', border=NA) +
  scale_x_discrete(limits = c("Source", "Target"), expand = c(.05, .05)) +
  scale_y_continuous(name="", breaks=NULL)





nodecolors <- c(rep('grey',23),wp[c(5:1)],rep('grey',22),wp[c(5:1)])
edgecolors <- as.character(edgelist_clusters$Target)
edgecolors[edgecolors=="1"] <- wp[1]
edgecolors[edgecolors=="2"] <- wp[2]
edgecolors[edgecolors=="3"] <- wp[3]
edgecolors[edgecolors=="4"] <- wp[4]
edgecolors[edgecolors=="5"] <- wp[5]
edgecolors[! edgecolors %in% wp] <- 'grey'
edgecolors <- c(edgecolors,edgecolors)

ggplot(edgelist_clusters,
       aes(y = Weight, axis1 = Source, axis2 = Target)) +
  geom_alluvium(fill = edgecolors, width=1/12) + #aes(fill = Admit), width = 1/12) +
  geom_stratum(fill = nodecolors, width = 1/12, lwd=.25, color = "#fafafa") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), label=c(rep('',23),c(5:1),rep('',22),c(5:1)), fill=NA, color='white', border=NA) +
  scale_x_discrete(limits = c("Source", "Target"), expand = c(.05, .05)) +
  scale_y_continuous(name="", breaks=NULL)
ggsave('figures/alluvial.png', width=4, height=6)

