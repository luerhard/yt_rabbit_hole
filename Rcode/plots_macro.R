library('ggplot2')
library('patchwork')
library('stargazer')
library('plyr')
library('tidyverse')
library('ggpubr')

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

## ASSIGN COLOR function ----

plotcol <- function(df, var, grouping='category', colpal=palette4){
  # calculate group average
  group_avg <- aggregate(df[[var]], by=list(df[[grouping]]), FUN=mean)
  # make color variable in df
  df$color <- 'white'
  # make color 'grey20' if value is above group average
  for (i in 1:nrow(group_avg)){
    if(group_avg[i,2] > 0){
      df$color[df[[grouping]] == group_avg[i,1] & df[[var]] > group_avg[i,2]] <- colpal[i]
      df$color[df[[grouping]] == group_avg[i,1] & df[[var]] < 0] <- colpal[i]
    } else {
      df$color[df[[grouping]] == group_avg[i,1] & df[[var]] < group_avg[i,2]] <- colpal[i]
      df$color[df[[grouping]] == group_avg[i,1] & df[[var]] > 0] <- colpal[i]
    }
  }
  return(df)
}

## PLOTTING ----

### network structural characteristics ----

ggplot(df, aes(x=category, y=size, label=name)) +
  #geom_boxplot(fill=palette4, alpha=.7) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('# of videos (LCC)') +
  geom_text(color = plotcol(df, 'size')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=avg_degree_clean, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('average degree (clean)') +
  geom_text(color = plotcol(df, 'avg_degree_clean')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=size_clean, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('# of videos (clean)') +
  geom_text(color = plotcol(df, 'size_clean')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=modularity, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('modularity') +
  geom_text(color = plotcol(df, 'modularity')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=viewcount, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('total # of views') +
  geom_text(color = plotcol(df, 'viewcount')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=gini_clean, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('gini of viewcount (clean)') +
  geom_text(color = plotcol(df, 'gini_clean')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=gini, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('gini of viewcount (LCC)') +
  geom_text(color = plotcol(df, 'gini')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=hub10_distance, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('hub distance') +
  geom_text(color = plotcol(df, 'hub10_distance')$color, size=2.5, alpha=.8)

ggplot(df, aes(x=category, y=no_clusters, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color='black', lwd=.25) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
  ylab('hub distance') +
  geom_text(color = plotcol(df, 'no_clusters')$color, size=2.5, alpha=.8)


ggplot(df, aes(x=size, y=modularity, shape=category, color=category)) +
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, alpha=.125, size=.5) +
  geom_point(size=2.5, fill="grey80") +
  scale_color_manual(values=c("blue","purple","orange","red")) +
  scale_shape_manual(values=c(1,2,6,5)) +
  ylim(c(-0.5,1)) +
  xlim(c(0,600))
  #scale_shape_manual(values=c(21,22,23,25))

(p2 <- ggplot(df, aes(x=category, y=no_clusters, label=name)) +
    stat_summary(fun = mean, geom = "bar", fill=palette4, color=palette4, lwd=.25) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
    labs(x=NULL, y=NULL, subtitle='Clusters') +
    geom_text(color = plotcol(df, 'no_clusters')$color, size=2, alpha=.5) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25))
ggsave("../../plots/macro_clusters.png", height=3.5, width=2.5, dpi=300)


### plots for macro section ----

levels(df$category)[levels(df$category)=="Non-controversial"] <- "NC"

# included in macro-level section: density, 
df <- df[order(df$category),]
(p1 <- ggplot(df, aes(x=category, y=avg_degree, label=name)) +
    #stat_summary(fun = mean, geom = "bar", fill=palette4, color=palette4, lwd=.25) + 
    #geom_text(color = plotcol(df, 'avg_degree')$color, size=2, alpha=.5) +
    #stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
    geom_boxplot(color=palette4, fill=palette4, alpha=.1, lwd=0.5, outlier.alpha=1) +
    stat_compare_means(method = "t.test", label = "p.signif", size = 3, label.y=c(19.2, 20.8,22.4),
      comparisons = list(c("News","Conspiracy"), c("Science","Conspiracy"), c("NC","Conspiracy"))) +
    scale_y_continuous(breaks=seq(0,20,5), limits=c(min(df$avg_degree),24.2)) +
    labs(x=NULL, y=NULL, subtitle='Density (avg degree)'))
ggsave("../../plots/macro_degree.png", height=3.5, width=2.5, dpi=300)

(p2 <- ggplot(df, aes(x=category, y=gini, label=name)) +
    #stat_summary(fun = mean, geom = "bar", fill=palette4, color=palette4, lwd=.25) +
    #geom_text(color = plotcol(df, 'gini')$color, size=2, alpha=.5) +
    #stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) + 
    geom_boxplot(color=palette4, fill=palette4, alpha=.1, lwd=0.5, outlier.alpha=1) +
    stat_compare_means(method = "t.test", label = "p.signif", size = 3, label.y=c(0.92,0.945,0.97),
      comparisons = list(c("News","Conspiracy"), c("Science","Conspiracy"), c("NC","Conspiracy"))) +
    scale_y_continuous(limits=c(min(df$gini),1)) +
    labs(x=NULL, y=NULL, subtitle='Centralization'))
ggsave("../../plots/macro_gini.png", height=3.5, width=2.5, dpi=300)

(p3 <- ggplot(df, aes(x=category, y=modularity, label=name)) +
    #stat_summary(fun = mean, geom = "bar", fill=palette4, color=palette4, lwd=.25) +
    #geom_text(color = plotcol(df, 'modularity')$color, size=2, alpha=.5)) #+ stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25))
    #stat_summary(fun.data = mean_se, geom = "errorbar", width=.2, lwd=.25) +
    geom_boxplot(fill=palette4, color=palette4, lwd=.5, alpha=.1, outlier.alpha=1) +
    #geom_text(color = rep(palette4,each=10), size=2, alpha=.5) +
    stat_compare_means(method = "t.test", label = "p.signif", size = 3, label.y=c(0.6,0.635,0.67),
      comparisons = list(c("News","Conspiracy"), c("Science","Conspiracy"), c("NC","Conspiracy"))) +
    scale_y_continuous(name="Modularity", limits=c(min(df$modularity),0.705)) +
    labs(x=NULL, y=NULL, subtitle='Modularity'))
ggsave("../../plots/macro_modularity.png", height=3.5, width=2.5, dpi=300)

library("ggConvexHull")
(p4 <- ggplot(df, aes(y=modularity, x=avg_degree, label=name, color=category)) +
  geom_convexhull(alpha = 0.1, lwd=.5, aes(fill = category)) + 
  geom_point(color=rep(palette4,each=10)) +
  #geom_text(color = rep(palette4,each=10), size=2, alpha=.5, hjust=-0.15, angle=15) +
  scale_y_continuous(name="Modularity", limits=c(0.22,0.6)) +
  scale_x_continuous(name="Density", limits=c(2,21)) +
  scale_fill_manual(values=palette4, name="") +
  scale_color_manual(values=palette4, name="") +
  ggtitle(NULL,"Modularity by density") +
  theme(legend.position = 'none'))

(p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = 'A') & theme(plot.tag.position  = c(.98, .98), plot.tag = element_text(face="bold"))
ggsave("../../plots/macro.png", height=5, width=4.6, dpi=300)

### sentiment ----




# included in micro-level section
ggplot(df, aes(x=category, y=avg_sentiment, label=name)) +
  stat_summary(fun = mean, geom = "bar", fill=palette4, color=palette4, lwd=.25) + 
  geom_abline(intercept=0, slope=0, size=0.25) +
  labs(x=NULL, y=NULL, subtitle='Average sentiment') +
  geom_text(color = plotcol(df, 'avg_sentiment')$color, size=2.5, alpha=.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width=.1, lwd=.25)
ggsave("../../plots/micro_sentiment.png", height=3, width=3, dpi=300)