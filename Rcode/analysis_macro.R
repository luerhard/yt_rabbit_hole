library('ggplot2')

theme_set(
  theme_classic() +
  theme(text=element_text(size=12, color='black', family="Open Sans"),
        axis.text=element_text(size=12, color='black', family="Open Sans"))
)

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

df$category[df$name %in% conspiracy_topics] <- "conspiracy"
df$category[df$name %in% noncontroversial_topics] <- "non-controversial"
df$category[df$name %in% news_topics] <- "news"
df$category[df$name %in% science_topics] <- "science"

df$category <- factor(df$category, levels=c("non-controversial","science","news","conspiracy"))

df$name <- gsub("_"," ",df$name)

## PLOTTING ----

ggplot(df, aes(x=category, y=size, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('# of videos (LCC)') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=avg_degree_clean, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('average degree (clean)') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=avg_degree, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('average degree (LCC)') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=size_clean, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('# of videos (clean)') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=isolates, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('# isolates') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=modularity, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('modularity') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=viewcount, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('total # of views') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=gini_clean, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('gini of viewcount (clean)') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=gini, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('gini of viewcount (LCC)') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=avg_sentiment, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('average sentiment') +
  geom_text(color = '#658DA0', size=3, alpha=1)

ggplot(df, aes(x=category, y=hub10_distance, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill='#BED1DB', color='black') + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2) +
  ylab('hub distance') +
  geom_text(color = '#658DA0', size=3, alpha=1)



## STATISTICAL MODELS ----

summary(lm(gini ~ category + viewcount + size, data=df))
summary(lm(avg_degree ~ category + viewcount + size, data=df))
# problems: viewcount on video level related to indegree, 

