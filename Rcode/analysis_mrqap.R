library('igraph')
library('ggplot2')
library('ggraph')
library('patchwork')
library('colorspace')
library('statnet')
library('stringr')
require('Rglpk')
library("grid")

source('theme_ggplot.R')

## CONSTANTS and names ----

# create complete nodeslist

network_names <- c(
  "how_to_draw","ab_workout","warrior_cats",
  "vintage_jewelry","pokemon_go","minecraft","wordle","van_life_us",
  "power_tools","urban_gardening",
  "game_theory","filter_bubbles","nft","climate_change",
  "monkeypox_virus","nanotechnology","blockchain","machine_learning","autism",
  "tourette_syndrome",
  "dominion_voting_system","roe_v_wade","critical_race_theory",
  "johnny_depp_amber_heard_trial","vaccine_mandate","derek_chauvin",
  "baby_formula","transgender","antifa","gas_prices",
  "plandemic","5g_covid","is_earth_flat","pizzagate",
  "adrenochrome","qanon","chemtrails","great_replacement_theory",
  "9_11_building_7","death_elvis_presley"
)

conspiracy_topics <- c(
  "plandemic","5g_covid","is_earth_flat","pizzagate","adrenochrome","qanon",
  "chemtrails","great_replacement_theory","9_11_building_7","death_elvis_presley")
noncontroversial_topics <- c(
  "how_to_draw","ab_workout","warrior_cats","vintage_jewelry","pokemon_go",
  "minecraft","wordle","van_life_us","power_tools","urban_gardening")
news_topics <- c(
  "dominion_voting_system","roe_v_wade","critical_race_theory","johnny_depp_amber_heard_trial",
  "vaccine_mandate","derek_chauvin","baby_formula","transgender","antifa","gas_prices")
science_topics <- c(
  "game_theory","filter_bubbles","nft","climate_change","monkeypox_virus",
  "nanotechnology","blockchain","machine_learning","autism","tourette_syndrome")

topic_df <- data.frame(
  category=c(rep("Non-controversial",10),rep("Science",10),rep("News",10),rep("Conspiracy",10)),
  topic=c(noncontroversial_topics,science_topics,news_topics,conspiracy_topics),
  color=rep(c("#4F3F84","#82AC26","#FFA22A","#FF662A"),each=10)
)
topic_df$category <- factor(topic_df$category, levels=c("Non-controversial","Science","News","Conspiracy"))

net_name <- "game_theory"

#network_names <- network_names[order(network_names)]



## FUNCTIONS ----

complete_timevec <- function(g){
  for(i in c(1:length(V(g)))){
    if(lengths(regmatches(V(g)$duration[i], gregexpr(":", V(g)$duration[i]))) < 2){
      V(g)$duration[i] <- paste0("0:",V(g)$duration[i])
    }
    if(str_sub(V(g)$duration[i],-1,-1) == ":"){
      V(g)$duration[i] <- paste0(V(g)$duration[i],"0")
    }
  }
  return(g)
}

# FUN to read graphs
read_graphs <- function(net_name){
  g <- read_graph(paste0("../data/clean/networks/",net_name,".gml"), format = "gml")
  
  # work viewcount (to log)
  V(g)$viewcount_log <- log(V(g)$viewcount)
  
  # work duration (to seconds, log)
  V(g)$duration <- substr(V(g)$duration,3,999)
  V(g)$duration <- gsub("H",":", V(g)$duration)
  V(g)$duration <- gsub("M",":", V(g)$duration)
  V(g)$duration <- gsub("S","", V(g)$duration)
  
  g <- complete_timevec(g)
  
  basetime <- as.POSIXct("0:0:0", format="%H:%M:%S")
  V(g)$duration <- as.numeric(as.POSIXct(V(g)$duration, format="%H:%M:%S") - basetime)
  V(g)$duration <- as.numeric(V(g)$duration)
  V(g)$duration_log <- log(as.numeric(V(g)$duration))
  
  V(g)$network_id <- net_name
  return(g)
}



## FITTING MRQAP models ----

qap_coefs <- topic_df
qap_coefs$intercept <- NA
qap_coefs$viewcount <- NA
qap_coefs$sentiment <- NA
qap_coefs$p_intercept <- NA
qap_coefs$p_viewcount <- NA
qap_coefs$p_sentiment <- NA

for(i in 1:40){
  net_name <- network_names[i]
  cat(rep('=',i*2),rep('-',80-i*2),'\n',sep='')
  cat('MRQAP test [',i,'/40]\t',net_name,'\n',sep='')
  
  g <- read_graph(paste0("../data/clean/networks/",net_name,".gml"), format = "gml")
  
  V(g)$viewcount_log <- log(V(g)$viewcount)
  V(g)$viewcount_log[V(g)$viewcount_log==-Inf] <- 0
  
  y <- as.matrix(as_adjacency_matrix(g))
  x <- list(
    'viewcount (log)' = matrix(rep(V(g)$viewcount_log,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'sentiment score' = matrix(rep(V(g)$sentiment,each=vcount(g)), nrow=vcount(g), ncol=vcount(g))
  )
  
  # quick & dirty hack for those netlms that fail
  if(i!=1) model$coefficients <- model$pgreqabs <- c(NA, NA, NA)
  
  try(print(model <- netlm(y, x, nullhyp=c("qapspp"), reps=1000)))
  
  qap_coefs$intercept[i] <- model$coefficients[1]
  qap_coefs$viewcount[i] <- model$coefficients[2]
  qap_coefs$sentiment[i] <- model$coefficients[3]
  qap_coefs$p_intercept[i] <- model$pgreqabs[1]
  qap_coefs$p_viewcount[i] <- model$pgreqabs[2]
  qap_coefs$p_sentiment[i] <- model$pgreqabs[3]
  flush.console()
}

qap_coefs$sig_sentiment <- "no"
qap_coefs$sig_sentiment[qap_coefs$p_sentiment < 0.05] <- "yes"
qap_coefs$sig_viewcount <- "no"
qap_coefs$sig_viewcount[qap_coefs$p_viewcount < 0.05] <- "yes"

saveRDS(qap_coefs, "../data/analysis/qap_coefs.rds")
qap_coefs <- readRDS("../data/analysis/qap_coefs.rds")

# PLOTTING

(p1 <- ggplot(qap_coefs, aes(x=category, y=sentiment, shape=sig_sentiment, group=category)) +
  geom_abline(intercept=0, slope=0, color=palette4[4]) +
  geom_boxplot(color=palette4, outliers=FALSE) +
  geom_jitter(height=0, width=0.2, color=qap_coefs$color, alpha=.75, size=3) +
  scale_shape_manual(values=c(1, 16)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name="") +
  ggtitle(NULL,"Effects of sentiment") +
  coord_flip() +
  theme(legend.position = 'none'))
#ggsave("../../plots/qap_sentiment.png", width=4, height=2.5, dpi=300)

(p2 <- ggplot(qap_coefs, aes(x=category, y=viewcount, shape=sig_viewcount, group=category)) +
  geom_abline(intercept=0, slope=0, color=palette4[4]) +
  geom_boxplot(color=palette4, outliers=FALSE) +
  geom_jitter(height=0, width=0.2, color=qap_coefs$color, alpha=.75, size=3) +
  scale_shape_manual(values=c(16,1)) +
  scale_x_discrete(name="", labels=NULL) +
  scale_y_continuous(name="", limits=c(0,NA)) +
  ggtitle(NULL,"Effects of viewcount (log)") +
  coord_flip() +
  theme(legend.position = 'none', axis.ticks.y=element_blank()))
#ggsave("../../plots/qap_viewcount.png", width=4, height=2.5, dpi=300)

p1 + p2 + plot_annotation(tag_levels = "A") & theme(plot.tag.position  = c(.98, .98), plot.tag = element_text(face="bold"))

ggsave("../../plots/qap.png", width=5.2, height=2, dpi=300)





# sandbox ----

net_name <- network_names[5]
g <- read_graph(paste0("../data/clean/networks/",net_name,".gml"), format = "gml")

V(g)$viewcount_log <- log(V(g)$viewcount)
V(g)$viewcount_log[V(g)$viewcount_log==-Inf] <- 0

y <- as.matrix(as_adjacency_matrix(g))
x <- list(
  'viewcount (log)' = matrix(rep(V(g)$viewcount_log,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
  'sentiment score' = matrix(rep(V(g)$sentiment,each=vcount(g)), nrow=vcount(g), ncol=vcount(g))
)

model <- netlm(y, x, nullhyp=c("qapspp"), reps=100)
summary(model)

