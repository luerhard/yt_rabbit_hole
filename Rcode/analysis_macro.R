library('ggplot2')
library('patchwork')
library('stargazer')

source('theme_ggplot.R')

## READ DATA ----

df <- read.csv('../data/clean/metadata/metadata_networks.csv')

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

df$category[df$name %in% conspiracy_topics] <- "Conspiracy"
df$category[df$name %in% noncontroversial_topics] <- "Non-controversial"
df$category[df$name %in% news_topics] <- "News"
df$category[df$name %in% science_topics] <- "Science"

df$category <- factor(df$category, levels=c("Non-controversial","Science","News","Conspiracy"))

df$name <- gsub("_"," ",df$name)

## CREATE TABLE ----

sg_table <- df[order(df$category,df$name),c('name','size_clean','size','modularity','viewcount')]
sg_table$size <- paste0(sg_table$size, " (", round(sg_table$size / sg_table$size_clean * 100),"%)")

stargazer(
  sg_table,
  type="latex", summary=FALSE, rownames=FALSE, label="tab:networks",
  caption="Complete list of networks collected for the study"
)

## STATISTICAL MODELS ----

df$viewcount <- df$viewcount / 10000000
df$size <- df$size / 100

#df$category <- relevel(df$category, ref = 4)

summary(m_mod <- lm(modularity ~ category + log(viewcount) + size, data=df))
summary(m_deg <- lm(avg_degree ~ category + log(viewcount) + size, data=df))
#summary(m_gin <- lm(gini_clean       ~ category + log(viewcount) + size, data=df))
summary(m_gin <- lm(gini       ~ category + log(viewcount) + size, data=df))
# !!! add model on centralization? Is this possible?

stargazer(m_mod, m_deg, m_gin, type="text",
          title = c("OLS regression models on indicators of rabbit holes in the graphs"), 
          dep.var.labels = c("Modularity", "Average degree", "Gini of degree distribution"),
          
          omit.stat = c("adj.rsq","ser"))

stargazer(m_mod, m_deg, m_gin, type="latex", 
          title = c("OLS regression models on indicators of rabbit holes in the graphs"), 
          dep.var.labels = c("Modularity", "Average degree", "Gini of degree distribution"),
          omit.stat = c("adj.rsq","ser"),
          out="tab_macro_mods.tex")
  # problems: viewcount on video level related to indegree, 


centr_degree(g, loops=F)

summary(lm(modularity ~ avg_degree, data=df[df$category=="Conspiracy",]))
cor.test(df[df$category=="Conspiracy",]$modularity, df[df$category=="Conspiracy",]$avg_degree)
cor.test(df[df$category=="Science",]$modularity, df[df$category=="Science",]$avg_degree)
cor.test(df[df$category=="News",]$modularity, df[df$category=="News",]$avg_degree)
cor.test(df[df$category=="Non-controversial",]$modularity, df[df$category=="Non-controversial",]$avg_degree)
