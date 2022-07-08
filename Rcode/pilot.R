library('dplyr')
library('ggplot2')
library('patchwork')
library('igraph')
library('tm')         # to create wordcloud
library('wordcloud')  # to create wordcloud

## load data [snowball] ----

df <- read.csv('../data/raw/roe_v_wade_max_12_test.csv')

df$id <- c(1:nrow(df))

## mark (seed) origin of video [snowball] ----

# check origin of the video
df$origin <- 0
for (i in df$id[df$step != 0]) {
  if (df$step[i] == 1) { 
    df$origin[i] <- df$X[df$source_video_id[i] == df$video_id]
  } else {
    df$origin[i] <- df$origin[df$source_video_id[i] == df$video_id]
  }
}

table(df$origin)
# >   0   1   2   3   4   5   6   7   8   9  10  11 
# > 828 792 972 120 588 972 480 576 312 384 204 996 

## create igraph object [snowball] ----

df$Target <- df$video_id
df$Source <- df$source_video_id

g <- graph_from_data_frame(df, directed = TRUE)

## word clouds from origin [snowball] ----

wc_gen <- function(originInteger=1){
  text <- df$description[df$origin==originInteger]
  docs <- Corpus(VectorSource(text))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  remove_words_from_cloud <- c(
    "subscribe","like","bell","youtube","channel","video",
    "https","iplayer"
  )
  docs <- tm_map(docs, removeWords, remove_words_from_cloud)
  
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  text.df <- data.frame(word = names(words),freq=words)
  set.seed(174)
  wordcloud(words = text.df$word, freq = text.df$freq, min.freq = 10,
            max.words=200, random.order=FALSE, rot.per=0.35)
}


#png(file="../../plots/wc_0.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(0)
#dev.off()
#png(file="../../plots/wc_1.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(1)
#dev.off()
#png(file="../../plots/wc_2.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(2)
#dev.off()
#png(file="../../plots/wc_3.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(3)
#dev.off()
#png(file="../../plots/wc_4.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(4)
#dev.off()
#png(file="../../plots/wc_5.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(5)
#dev.off()
#png(file="../../plots/wc_6.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(6)
#dev.off()
#png(file="../../plots/wc_7.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(7)
#dev.off()
#png(file="../../plots/wc_8.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(8)
#dev.off()
#png(file="../../plots/wc_9.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(9)
#dev.off()
#png(file="../../plots/wc_10.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(10)
#dev.off()
#png(file="../../plots/wc_11.png",width=2,height=2,units="in",res=1200,pointsize=4)
#wc_gen(11)
#dev.off()



## load data [catch all] ----

df <- read.csv('../data/raw/roe_v_wade_catch_all.csv')

df <- df[df$video_id %in% unique(df$source_video_id) & df$video_id != "",]

nodeslist <- df[,c(2:6)]
nodeslist <- nodeslist[match(unique(nodeslist$video_id), nodeslist$video_id),]
colnames(nodeslist)[1] <- c("Source")
edgelist <- df[,c(8,2)]
colnames(edgelist) <- c("Source","Target")
edgelist <- edgelist[edgelist$Source %in% nodeslist$Source, ]
edgelist <- edgelist[edgelist$Target %in% nodeslist$Source, ]

## create igraph object [catch all] ----

g <- graph_from_data_frame(edgelist, directed = TRUE, vertices = nodeslist)
components <- components(g)

g <- induced_subgraph(g, vids = V(g)[components$membership %in% which.max(components$csize)])

#cluster_optimal(g) # takes long
cl <- cluster_louvain(as.undirected(g))


V(g)$cluster <- cl$membership

plot(g, vertex.color=V(g)$cluster, vertex.label=NA, vertex.size=4, edge.arrow.size=0.5)
