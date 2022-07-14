library('ggplot2')

theme_set(
  theme_classic() +
  theme(text=element_text(size=12, color='black'),
        axis.text=element_text(size=12, color='black'))
)

df <- read.csv('../data/clean/metadata/metadata_networks.csv')

conspiracy_topics <- c("plandemic","5g_covid","is_earth_flat","pizzagate",
  "adrenochrome","qanon","chemtrails","great_replacement_theory",
  "9_11_building_7","death_elvis_presley")
noncontroversial_topics <- c("how_to_draw","ab_workout","warrior_cats",
                             "vintage_jewelry","pokemon_go","minecraft",
                             "wordle","van_life_us","power_tools",
                             "urban_gardening")
news_topics <- c("dominion_voting_system","roe_v_wade","critical_race_theory",
                 "johnny_depp_amber_heard_trial","vaccine_mandate",
                 "derek_chauvin","baby_formula","transgender","antifa",
                 "gas_prices")
science_topics <- c("game_theory","filter_bubbles","nft","climate_change",
                    "monkeypox_virus","nanotechnology","blockchain",
                    "machine_learning","autism","tourette_syndrome")

df$category[df$name %in% conspiracy_topics] <- "conspiracy"
df$category[df$name %in% noncontroversial_topics] <- "noncontroversial"
df$category[df$name %in% news_topics] <- "news"
df$category[df$name %in% science_topics] <- "science"

ggplot(df, aes(x=category, y=size_largest_component, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='white', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  xlab('') +
  geom_text(color = 'blue')

ggplot(df, aes(x=category, y=avg_degree_clean, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='white', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  geom_text(color = 'blue') +
  theme_classic()

ggplot(df, aes(x=category, y=size_clean)) +
  stat_summary(fun = mean, geom = "bar", fill='white', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  theme_classic()

ggplot(df, aes(x=category, y=isolates)) +
  stat_summary(fun = mean, geom = "bar", fill='white', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  theme_classic()

ggplot(df, aes(x=category, y=modularity, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='white', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  geom_text(color = 'blue') +
  theme_classic()







g <- read_graph("../data/clean/networks/roe_v_wade.gml", format='gml')
V(g)$title[V(g)$label == "9HZj8Qp4p2A"]
