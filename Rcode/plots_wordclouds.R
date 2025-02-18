# making wordclouds from video descriptions or titles

library('dplyr')
library('tm')
library('wordcloud')

# import (clean) data

g <- read_graph('../data/clean/networks/filter_bubbles.gml', format = "gml")

# function for wordcloud plotting

wc_gen <- function(cluster_id=1, analyze_text="description", min_freq=5){
  # from descriptions/titles to plain text
  if(analyze_text == "description"){
    text <- V(g)$description[V(g)$cluster==cluster_id]
  } else if(analyze_text == "title"){
    text <- V(g)$title[V(g)$cluster==cluster_id]
  }
  docs <- Corpus(VectorSource(text))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  
  # remove common words
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
  wordcloud(words = text.df$word, freq = text.df$freq, min.freq = min_freq,
            max.words=200, random.order=FALSE, rot.per=0.35)
}

# plot some clouds

wc_gen(1)
wc_gen(2)
wc_gen(3)
wc_gen(4)
wc_gen(5)
wc_gen(6)

wc_gen(1, "title")
wc_gen(2, "title")
wc_gen(3, "title")
wc_gen(4, "title")
wc_gen(5, "title")
wc_gen(6, "title")

