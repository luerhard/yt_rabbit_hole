# analysis micro

library('igraph')
library('ggplot2')
library('lme4')

## LOAD DATA ----

network_files <- list.files(path="../data/clean/networks", pattern="*.gml", full.names=FALSE, recursive=FALSE)
df <- data.frame()

for(i in network_files){
  g <- read_graph(paste0('../data/clean/networks/',i), format="gml")
  temp <- igraph::as_data_frame(g, "vertices")
  temp$name <- substr(i,1,nchar(i)-4)
  df <- rbind(df, temp)
  rm(temp)
}

conspiracy_topics <- c("plandemic","5g_covid","is_earth_flat","pizzagate",
                       "adrenochrome","qanon","chemtrails","great_replacement_theory",
                       "9_11_building_7","death_elvis_presley")
noncontroversial_topics <- c("how_to_draw","ab_workout","warrior_cats",
                             "vintage_jewelry","pokemon_go","minecraft","wordle","van_life_us",
                             "power_tools","urban_gardening")
news_topics <- c("dominion_voting_system","roe_v_wade","critical_race_theory",
                 "johnny_depp_amber_heard_trial","vaccine_mandate","derek_chauvin",
                 "baby_formula","transgender","antifa","gas_prices")
science_topics <- c("game_theory","filter_bubbles","nft","climate_change",
                    "monkeypox_virus","nanotechnology","blockchain","machine_learning","autism",
                    "tourette_syndrome")

df$category[df$name %in% conspiracy_topics] <- "conspiracy"
df$category[df$name %in% noncontroversial_topics] <- "non-controversial"
df$category[df$name %in% news_topics] <- "news"
df$category[df$name %in% science_topics] <- "science"

df$category <- factor(df$category, levels=c("non-controversial","science","news","conspiracy"))

df$name <- gsub("_"," ",df$name)

df$viewcount <- df$viewcount / 10^4

## ANALYSIS ----

summary(lm(indegree ~ sentiment + viewcount, df))

mlm <- lmer(indegree ~ sentiment + viewcount + (1 | category) + (0 + sentiment | category), df)
summary(mlm)
