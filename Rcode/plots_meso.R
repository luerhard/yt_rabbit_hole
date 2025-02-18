# analysis meso

library('igraph')
library('ggplot2')
library('patchwork')
library('ggalluvial')
library('dplyr')

theme_set(
  theme_linedraw() +
    theme(text=element_text(size=8, color='black', family="Open Sans"),
          plot.subtitle = element_text(size=9, color='black', family="Open Sans"),
          axis.text=element_text(size=8, color='black', family="Open Sans"),
          legend.text=element_text(size=8, color='black', family="Open Sans"),
          legend.position="bottom",panel.grid=element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))
)

palette3 <- c("#B11225", "#512888", "#262A77")
palette3 <- c("blue3","purple3","red3")
#palette3 <- rev(c("#FF662A", "#82AC26", "#4F3F84"))
stratumfill <- c('grey90',rev(palette3),'grey90',rev(palette3))

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

channel_categories <- c("AntiSJW","AntiTheist","Black","Conspiracy","Educational","LateNightTalkShow","LGBT","Libertarian","MainstreamNews","MissingLinkMedia","MRA","OrganizedReligion","PartisanLeft","PartisanRight","Politician","QAnon","ReligiousConservative","Socialist","SocialJustice","StateFunded","WhiteIdentitarian")

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

nodes$leftright <- factor(nodes$leftright, levels=c("L","C","R","NA"))

## ALLUVIAL leftright ----

lr_connect <- merge(data.frame(source=edges$source, target=edges$target),
                    data.frame(source=nodes$label, source_lr=nodes$leftright, 
                               network=nodes$name, category=nodes$category,
                               weight=1),
                    by='source')
lr_connect <- merge(lr_connect,
                    data.frame(target=nodes$label, target_lr=nodes$leftright),
                    by='target')
lr_connect$source_lr[is.na(lr_connect$source_lr)] <- "NA"
lr_connect$source_lr[lr_connect$source_lr==""] <- "NA"
lr_connect$target_lr[is.na(lr_connect$target_lr)] <- "NA"
lr_connect$target_lr[lr_connect$target_lr==""] <- "NA"

lr_connect <- aggregate(weight ~ source_lr + target_lr + category, data=lr_connect, FUN=sum)

lr_connect <- lr_connect[!(lr_connect$source_lr=="NA" & lr_connect$target_lr=="NA"),]


## News

df_news <- lr_connect[lr_connect$category=="news",]

edgecolors <- as.character(df_news$target_lr)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_news$source_lr <- factor(df_news$source_lr, levels = c("L","C","R","NA"))
df_news$target_lr <- factor(df_news$target_lr, levels = c("L","C","R","NA"))

ggplot(df_news,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.8) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.25) +
  geom_stratum(width=2/12, fill=stratumfill, color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="News")
ggsave('../../plots/meso_alluvial_news.png', width=2.5, height=3.5, dpi=300)


## Science

df_science <- lr_connect[lr_connect$category=="science",]

edgecolors <- as.character(df_science$target_lr)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_science$source_lr <- factor(df_science$source_lr, levels = c("L","C","R","NA"))
df_science$target_lr <- factor(df_science$target_lr, levels = c("L","C","R","NA"))

ggplot(df_science,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=1) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.5) +
  geom_stratum(width=2/12, fill=stratumfill, color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="Science")
ggsave('../../plots/meso_alluvial_science.png', width=2.5, height=3.5, dpi=300)


## Non-controversial

df_nc <- lr_connect[lr_connect$category=="non-controversial",]

edgecolors <- as.character(df_nc$target_lr)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_nc$source_lr <- factor(df_nc$source_lr, levels = c("L","C","R","NA"))
df_nc$target_lr <- factor(df_nc$target_lr, levels = c("L","C","R","NA"))

ggplot(df_nc,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=1) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.5) +
  geom_stratum(width=2/12, fill=rep(c('grey90',palette3[2:1]),2), color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="Non-controversial")
ggsave('../../plots/meso_alluvial_nc.png', width=2.5, height=3.5, dpi=300)


## Conspiracy

df_conspiracy <- lr_connect[lr_connect$category=="conspiracy",]

edgecolors <- as.character(df_conspiracy$target_lr)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_conspiracy$source_lr <- factor(df_conspiracy$source_lr, levels = c("L","C","R","NA"))
df_conspiracy$target_lr <- factor(df_conspiracy$target_lr, levels = c("L","C","R","NA"))

ggplot(df_conspiracy,
       aes(y = weight, axis1 = source_lr, axis2 = target_lr, label=source_lr)) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=1) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.5) +
  geom_stratum(width=2/12, fill=stratumfill, color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="Conspiracy")
ggsave('../../plots/meso_alluvial_conspiracy.png', width=2.5, height=3.5, dpi=300)



## ALLUVIAL category ----

nodes <- nodes[,c("id","label","step","viewcount","likecount","commentcount","duration","indegree","sentiment","cluster","leftright","AntiSJW","AntiTheist","Black","Conspiracy","Educational","LateNightTalkShow","LGBT","Libertarian","MainstreamNews","MissingLinkMedia","MRA","OrganizedReligion","PartisanLeft","PartisanRight","Politician","QAnon","ReligiousConservative","Socialist","SocialJustice","StateFunded","WhiteIdentitarian","name","category")]

nodes$None <- rowSums(nodes[,channel_categories])
nodes$None <- ifelse(nodes$None==0,1,0)

nodes <- reshape2::melt(
  nodes, measure.vars=channel_cateories,
)
nodes <- nodes %>% rename(ledzai_cat="variable")
nodes$ledzai_cat <- as.character(nodes$ledzai_cat)
nodes <- nodes[nodes$value==1,]

table(nodes$ledzai_cat)

cat_connect <- merge(
  data.frame(source=edges$source, target=edges$target),
  data.frame(source=nodes$label, category=nodes$category, network=nodes$name, source_cat=nodes$ledzai_cat, weight=1),
  by="source"
)
cat_connect <- merge(
  cat_connect,
  data.frame(target=nodes$label, target_cat=nodes$ledzai_cat),
  by="target"
)
cat_connect$source_cat[cat_connect$source_cat=="None"] <- "NA"
cat_connect$target_cat[cat_connect$target_cat=="None"] <- "NA"

cat_connect <- aggregate(weight ~ source_cat + target_cat + category, data=cat_connect, FUN=sum)

cat_connect <- cat_connect[!(cat_connect$source_cat=="NA" & cat_connect$target_cat=="NA"),]



ggplot(cat_connect,
       aes(y = weight, axis1 = source_cat, axis2 = target_cat, label=source_cat)) +
  geom_alluvium(width=1/12, lwd=0, alpha=0.8) +
  geom_alluvium(width=1/12, lwd=0, alpha=0.25) +
  geom_stratum(width=2/12, color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="Total")



## News

df_news <- cat_connect[cat_connect$category=="news",]

edgecolors <- as.character(df_news$target_cat)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_news$source_cat <- factor(df_news$source_cat, levels = channel_categories)
df_news$target_cat <- factor(df_news$target_cat, levels = channel_categories)

ggplot(df_news,
       aes(y = weight, axis1 = source_cat, axis2 = target_cat, label=source_cat)) +
  geom_alluvium(fill='blue', width=1/12, color='white', lwd=0, alpha=0.8) +
  geom_alluvium(fill='blue', width=1/12, color='white', lwd=0, alpha=0.25) +
  geom_stratum(width=2/12, fill='blue', color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="News")
ggsave('../../plots/meso_alluvial_news.png', width=2.5, height=3.5, dpi=300)


## Science

df_science <- cat_connect[cat_connect$category=="science",]

edgecolors <- as.character(df_science$target_cat)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_science$source_cat <- factor(df_science$source_cat, levels = c("L","C","R","NA"))
df_science$target_cat <- factor(df_science$target_cat, levels = c("L","C","R","NA"))

ggplot(df_science,
       aes(y = weight, axis1 = source_cat, axis2 = target_cat, label=source_cat)) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=1) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.5) +
  geom_stratum(width=2/12, fill=stratumfill, color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="Science")
ggsave('../../plots/meso_alluvial_science.png', width=2.5, height=3.5, dpi=300)


## Non-controversial

df_nc <- cat_connect[cat_connect$category=="non-controversial",]

edgecolors <- as.character(df_nc$target_cat)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_nc$source_cat <- factor(df_nc$source_cat, levels = c("L","C","R","NA"))
df_nc$target_cat <- factor(df_nc$target_cat, levels = c("L","C","R","NA"))

ggplot(df_nc,
       aes(y = weight, axis1 = source_cat, axis2 = target_cat, label=source_cat)) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=1) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.5) +
  geom_stratum(width=2/12, fill=rep(c('grey90',palette3[2:1]),2), color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="Non-controversial")
ggsave('../../plots/meso_alluvial_nc.png', width=2.5, height=3.5, dpi=300)


## Conspiracy

df_conspiracy <- cat_connect[cat_connect$category=="conspiracy",]

edgecolors <- as.character(df_conspiracy$target_cat)
edgecolors[edgecolors=="NA"] <- "grey90"
edgecolors[edgecolors=="L"] <- palette3[1]
edgecolors[edgecolors=="C"] <- palette3[2]
edgecolors[edgecolors=="R"] <- palette3[3]
edgecolors <- c(edgecolors,edgecolors)

df_conspiracy$source_cat <- factor(df_conspiracy$source_cat, levels = c("L","C","R","NA"))
df_conspiracy$target_cat <- factor(df_conspiracy$target_cat, levels = c("L","C","R","NA"))

ggplot(df_conspiracy,
       aes(y = weight, axis1 = source_cat, axis2 = target_cat, label=source_cat)) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=1) +
  geom_alluvium(fill=edgecolors, width=1/12, color='white', lwd=0, alpha=0.5) +
  geom_stratum(width=2/12, fill=stratumfill, color='black', lwd=0.25) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color='white') +
  theme(panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank()) +
  labs(subtitle="Conspiracy")
ggsave('../../plots/meso_alluvial_conspiracy.png', width=2.5, height=3.5, dpi=300)



## DESCRIPTIVES ----

ledzai <- read.csv("../data/external/recfluence_channel_review.csv")
colnames(ledzai) <- gsub("_","",tolower(colnames(ledzai)))

nodes$leftright[nodes$leftright %in% c("","Inf")] <- NA

nodes$leftright <- factor(nodes$leftright, levels=c("L","C","R"))

nodes$category <- as.character(nodes$category)
nodes$category[nodes$category=="non-controversial"] <- "NC"
nodes$category[nodes$category=="science"] <- "Science"
nodes$category[nodes$category=="news"] <- "News"
nodes$category[nodes$category=="conspiracy"] <- "Conspiracy"
nodes$category <- factor(nodes$category, levels=c("NC","Science","News","Conspiracy"))

percs <- c(
  sum(nodes$leftright=="L", na.rm=T) / nrow(nodes),
  sum(nodes$leftright=="C", na.rm=T) / nrow(nodes),
  sum(nodes$leftright=="R", na.rm=T) / nrow(nodes),
  sum(!nodes$leftright %in% c("L","C","R"), na.rm=T) / nrow(nodes))

percs <- paste0(round(percs*100,2),"%")

percs 
cat(
  'Number of videos labelled:\t', sum(nodes$leftright %in% c("L","C","R"), na.rm=T),
  '\nOut of a total of:\t\t', nrow(nodes),
  '\nAccounting for number of views:\t', sum(nodes$viewcount[nodes$leftright %in% c("L","C","R")], na.rm=T),
  '\nOut of a total of:\t\t', sum(nodes$viewcount, na.rm=T))

ggplot(nodes, aes(x=leftright, fill=leftright)) +
  geom_bar(color=c(palette3,'grey90'),lwd=.25) +
  geom_text(label=percs, y=0, stat= "count", color='black',vjust = 1.2, size=2) +
  scale_fill_manual(name="Left-right", values=palette3, na.value = "grey90") +
  ylim(-200,NA) + 
  labs(x="Left-right", y="", subtitle="Number of videos classified by channel leaning") +
  theme(legend.position = "none")
ggsave('../../plots/meso_lr.png', width=3.5, height=2.25, dpi=300)

ggplot(nodes, aes(x=category, fill=leftright)) +
  geom_bar(color=c(palette3[1:2],'grey90',rep(c(palette3,'grey90'),3)),lwd=.25) +
  scale_fill_manual(name="Left-right", values=palette3, na.value = "grey90") +
  labs(x="Category",y="",subtitle="Number of videos classified by category")+
  theme(legend.position = "none")
ggsave('../../plots/meso_lrCategory.png', width=3.5, height=2.25, dpi=300)

aggdf <- nodes
aggdf$leftright <- factor(aggdf$leftright, levels=c("L","C","R","NA"))
aggdf$leftright[is.na(aggdf$leftright)] <- "NA"
aggdf <- aggregate(viewcount ~ category + leftright, data=aggdf, FUN=sum)

aggdf <- aggdf[order(aggdf$category,aggdf$leftright),]

ggplot(aggdf, aes(x=category, fill=leftright, y=viewcount)) +
  geom_bar(stat='identity',color=c(palette3[1:2],'grey90',rep(c(palette3,'grey90'),3)),lwd=.25) +
  scale_fill_manual(name="Left-right", values=c(palette3,'grey90'), na.value = "grey90") +
  labs(subtitle="Summed viewcount by category", x="Category", y="")
ggsave('../../plots/meso_lrViewcount.png', width=3.5, height=2.75, dpi=300)

# resetting variable as character
nodes$leftright <- as.character(nodes$leftright)
nodes$leftright[nodes$leftright=="NA"] <- NA


