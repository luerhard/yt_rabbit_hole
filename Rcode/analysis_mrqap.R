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

channel_categories <- c("AntiSJW","AntiTheist","Black","Conspiracy","Educational","LateNightTalkShow","LGBT","Libertarian","MainstreamNews","MissingLinkMedia","MRA","OrganizedReligion","PartisanLeft","PartisanRight","Politician","QAnon","ReligiousConservative","Socialist","SocialJustice","StateFunded","WhiteIdentitarian")

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

### SIMPLE MODEL (original) ----

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

ggsave("../../plots/qap.png", width=4.6, height=2, dpi=300)


### FULL MODEL ----

qap_coefs <- topic_df

qap_coefs$intercept <- NA
qap_coefs$viewcount_log <- NA
qap_coefs$sentiment <- NA
qap_coefs$comments <- NA
qap_coefs$likes <- NA
qap_coefs$duration <- NA
qap_coefs$same_channel <- NA
qap_coefs$same_leftright <- NA
qap_coefs$leftright_L <- NA
qap_coefs$leftright_C <- NA
qap_coefs$leftright_none <- NA
qap_coefs$same_cat <- NA
qap_coefs$AntiSJW <- NA
qap_coefs$AntiTheist <- NA
qap_coefs$Black <- NA
qap_coefs$Conspiracy <- NA
qap_coefs$Educational <- NA
qap_coefs$LateNightTalkShow <- NA
qap_coefs$LGBT <- NA
qap_coefs$Libertarian <- NA
qap_coefs$MainstreamNews <- NA
qap_coefs$MissingLinkMedia <- NA
qap_coefs$MRA <- NA
qap_coefs$OrganizedReligion <- NA
qap_coefs$PartisanLeft <- NA
qap_coefs$PartisanRight <- NA
qap_coefs$Politician <- NA
qap_coefs$QAnon <- NA
qap_coefs$ReligiousConservative <- NA
qap_coefs$Socialist <- NA
qap_coefs$SocialJustice <- NA
qap_coefs$StateFunded <- NA
qap_coefs$WhiteIdentitarian <- NA

qap_ps <- topic_df

qap_ps$intercept <- NA
qap_ps$viewcount_log <- NA
qap_ps$sentiment <- NA
qap_ps$comments <- NA
qap_ps$likes <- NA
qap_ps$duration <- NA
qap_ps$same_channel <- NA
qap_ps$same_leftright <- NA
qap_ps$leftright_L <- NA
qap_ps$leftright_C <- NA
qap_ps$leftright_none <- NA
qap_ps$same_cat <- NA
qap_ps$AntiSJW <- NA
qap_ps$AntiTheist <- NA
qap_ps$Black <- NA
qap_ps$Conspiracy <- NA
qap_ps$Educational <- NA
qap_ps$LateNightTalkShow <- NA
qap_ps$LGBT <- NA
qap_ps$Libertarian <- NA
qap_ps$MainstreamNews <- NA
qap_ps$MissingLinkMedia <- NA
qap_ps$MRA <- NA
qap_ps$OrganizedReligion <- NA
qap_ps$PartisanLeft <- NA
qap_ps$PartisanRight <- NA
qap_ps$Politician <- NA
qap_ps$QAnon <- NA
qap_ps$ReligiousConservative <- NA
qap_ps$Socialist <- NA
qap_ps$SocialJustice <- NA
qap_ps$StateFunded <- NA
qap_ps$WhiteIdentitarian <- NA

for(i in 1:40){
  net_name <- network_names[i]
  g <- read_graph(paste0("../data/clean/networks/",net_name,".gml"), format = "gml")
  
  V(g)$viewcount_log <- log(V(g)$viewcount)
  V(g)$viewcount_log[V(g)$viewcount_log==-Inf] <- 0
  
  V(g)$comments_normalized <- V(g)$commentcount / V(g)$viewcount
  V(g)$likes_normalized <- V(g)$likecount / V(g)$viewcount
  
  
  hms <- sapply(c('H', 'M', 'S'), function(unit) 
    sub(paste0('.*[^0-9]+([0-9]+)', unit, '.*'), '\\1', V(g)$duration))
  suppressWarnings(mode(hms) <- 'numeric')
  V(g)$duration <- colSums(t(hms) * 60^(2:0), na.rm=T)
  
  E(g)$same_channel <- NA
  for(e in 1:length(E(g))){
    E(g)$same_channel[e] <- as.integer(V(g)$channelid[ends(g,e)[1]] == V(g)$channelid[ends(g,e)[2]])
  }
  
  E(g)$same_channel_cat <- NA
  for(e in 1:length(E(g))){
    total_matches <- 0
    for(c in channel_categories){
      if((vertex_attr(g, c)[ends(g,e)[1]] == 1) & (vertex_attr(g, c)[ends(g,e)[2]] == 1)){
        total_matches <- total_matches + 1
      }
    }
    E(g)$same_channel_cat[e] <- as.integer(total_matches > 0)
  }
  
  E(g)$same_channel_leftright <- NA
  for(e in 1:length(E(g))){
    if(V(g)$leftright[ends(g,e)[1]] == V(g)$leftright[ends(g,e)[2]]){
      E(g)$same_channel_leftright[e] <- 1
    } else {
      E(g)$same_channel_leftright[e] <- 0
    }
  }
  
  V(g)$leftright_L <- as.integer(V(g)$leftright == "L")
  V(g)$leftright_C <- as.integer(V(g)$leftright == "C")
  V(g)$leftright_R <- as.integer(V(g)$leftright == "R")
  V(g)$leftright_none <- as.integer(V(g)$leftright == "FALSE")
  
  
  y <- as.matrix(as_adjacency_matrix(g))
  x <- list(
    'viewcount (log)' = matrix(rep(V(g)$viewcount_log,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'sentiment score' = matrix(rep(V(g)$sentiment,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'comments (normalized)' = matrix(rep(V(g)$comments_normalized,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'likes (normalized)' = matrix(rep(V(g)$likes_normalized,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'duration' = matrix(rep(V(g)$duration,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'same channel' = matrix(as_adjacency_matrix(g, attr="same_channel"), nrow=vcount(g), ncol=vcount(g)),
    'same leftright' = matrix(as_adjacency_matrix(g, attr="same_channel_leftright"), nrow=vcount(g), ncol=vcount(g)),
    'leftright | L' = matrix(rep(V(g)$leftright_L,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'leftright | C' = matrix(rep(V(g)$leftright_C,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'leftright | none' = matrix(rep(V(g)$leftright_none,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'same category' = matrix(as_adjacency_matrix(g, attr="same_channel_cat"), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | AntiSJW' = matrix(rep(V(g)$AntiSJW,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | AntiTheist' = matrix(rep(V(g)$AntiTheist,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | Black' = matrix(rep(V(g)$Black,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | Conspiracy' = matrix(rep(V(g)$Conspiracy,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | Educational' = matrix(rep(V(g)$Educational,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | LateNightTalkShow' = matrix(rep(V(g)$LateNightTalkShow,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | LGBT' = matrix(rep(V(g)$LGBT,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | Libertarian' = matrix(rep(V(g)$Libertarian,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | MainstreamNews' = matrix(rep(V(g)$MainstreamNews,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | MissingLinkMedia' = matrix(rep(V(g)$MissingLinkMedia,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | MRA' = matrix(rep(V(g)$MRA,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | OrganizedReligion' = matrix(rep(V(g)$OrganizedReligion,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | PartisanLeft' = matrix(rep(V(g)$PartisanLeft,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | PartisanRight' = matrix(rep(V(g)$PartisanRight,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | Politician' = matrix(rep(V(g)$Politician,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | QAnon' = matrix(rep(V(g)$QAnon,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | ReligiousConservative' = matrix(rep(V(g)$ReligiousConservative,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | Socialist' = matrix(rep(V(g)$Socialist,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | SocialJustice' = matrix(rep(V(g)$SocialJustice,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | StateFunded' = matrix(rep(V(g)$StateFunded,each=vcount(g)), nrow=vcount(g), ncol=vcount(g)),
    'channel cat | WhiteIdentitarian' = matrix(rep(V(g)$WhiteIdentitarian,each=vcount(g)), nrow=vcount(g), ncol=vcount(g))
  )
  
  # get index of lists that sum to 0
  index_to_use <- which(lapply(x, function(x) sum(x)) != 0)
  # select only those lists
  x <- x[index_to_use]
  
  model <- netlm(y, x, nullhyp=c("qapspp"), reps=1000)
  summary(model)
  qap_coefs[i, c(4,index_to_use+4)] <- model$coefficients
  qap_ps[i, c(4,index_to_use+4)] <- model$pgreqabs
}

saveRDS(qap_coefs, "../data/analysis/qap_coefs.rds")
qap_coefs <- readRDS("../data/analysis/qap_coefs.rds")
