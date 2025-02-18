library('dplyr')
library('igraph')
library('textclean')
library('DescTools')
library('tidyverse')
library('here')

sentiment_path <- '../data/interim/title_sentiments/'

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

## SENTIMENT ----

sentiment_df <- data.frame()

for(net_name in network_names){
  sentiment_df <- rbind(sentiment_df, read.csv(paste0(sentiment_path, net_name, '.csv')))
}

sentiment_df <- sentiment_df %>% rename(label = video_id)
sentiment_df <- unique(sentiment_df)
sentiment_df <- sentiment_df[!duplicated(sentiment_df$label),]

## RECFLUENCE ----

recfluence_df <- read.csv("../data/external/recfluence_channel_review.csv") %>% 
  select(
    CHANNEL_TITLE, TAGS, CHANNEL_ID, LR
  ) %>%
  rename(
    channelid = CHANNEL_ID
  )

recfluence_cats <- str_split(recfluence_df$TAGS, "\\|") %>% unlist() %>% unique()
recfluence_cats <- recfluence_cats[recfluence_cats != ""]

temp_df <- data.frame(matrix(ncol = length(recfluence_cats), nrow = nrow(recfluence_df)))
temp_df[is.na(temp_df)] <- 0
colnames(temp_df) <- recfluence_cats[order(recfluence_cats)]

recfluence_df <- cbind(
  recfluence_df, 
  temp_df
)

for(i in 1:nrow(recfluence_df)){
  tags <- str_split(recfluence_df$TAGS[i], "\\|") %>% unlist()
  tags <- tags[tags != ""]
  recfluence_df[i, tags] <- 1
}

# recode recfluence_df$LR to -1, 0, 1
recfluence_df$LR <- recode(recfluence_df$LR, "L" = -1, "C" = 0, "R" = 1)

# drop 'CHANNEL_TITLE','TAGS' from recfluence_df
recfluence_df <- recfluence_df %>% select(-c(CHANNEL_TITLE, TAGS))

# aggregate to keep unique channelids
recfluence_df <- recfluence_df %>% 
  group_by(channelid) %>% 
  summarise_all(mean)

# recode recfluence_df$LR to "L" "C" "R"
recfluence_df$LR <- recode(recfluence_df$LR, "-1" = "L", "0" = "C", "1" = "R")

# set all entries in columns 3:23 above 1 to TRUE
recfluence_df[,3:23] <- recfluence_df[,3:23] > 0


## MERGE WITH GRAPHS ----

for(net_name in network_names){
  print(paste('Reading graph',net_name))
  g <- read_graph(paste0("../data/clean/networks/",net_name,".gml"), format = "gml")
  # add vertex attributes from sentiment_df, matched on label
  V(g)$sentiment <- sentiment_df[sentiment_df$label %in% V(g)$label, 'sentiment']
  
  # add all vertex attributes from recfluence_df, matched on channelid
  vertexdf <- data.frame(channelid = V(g)$channelid)
  vertexdf <- merge(
    recfluence_df[recfluence_df$channelid %in% V(g)$channelid,],
    vertexdf, by='channelid', all=T
  )
  vertexdf[is.na(vertexdf)] <- FALSE
  
  V(g)$leftright <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'LR']
  V(g)$AntiSJW <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'AntiSJW']
  V(g)$AntiTheist <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'AntiTheist']
  V(g)$Black <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'Black']
  V(g)$Conspiracy <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'Conspiracy']
  V(g)$Educational <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'Educational']
  V(g)$LateNightTalkShow <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'LateNightTalkShow']
  V(g)$LGBT <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'LGBT']
  V(g)$Libertarian <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'Libertarian']
  V(g)$MainstreamNews <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'Mainstream News']
  V(g)$MissingLinkMedia <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'MissingLinkMedia']
  V(g)$MRA <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'MRA']
  V(g)$OrganizedReligion <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'OrganizedReligion']
  V(g)$PartisanLeft <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'PartisanLeft']
  V(g)$PartisanRight <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'PartisanRight']
  V(g)$Politician <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'Politician']
  V(g)$QAnon <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'QAnon']
  V(g)$ReligiousConservative <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'ReligiousConservative']
  V(g)$Socialist <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'Socialist']
  V(g)$SocialJustice <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'SocialJustice']
  V(g)$StateFunded <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'StateFunded']
  V(g)$WhiteIdentitarian <- vertexdf[vertexdf$channelid %in% V(g)$channelid, 'WhiteIdentitarian']
  
  write_graph(g, paste0("../data/clean/networks/",net_name,".gml"), format = "gml")
}

