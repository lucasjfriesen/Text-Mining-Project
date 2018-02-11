library(LSAfun)
library(lsa)
library(jsonlite)
library(tm)

tweets <- fromJSON("/Users/lucasfriesen/Google Drive/R/Whatarepeoplesaying/Collected data/merged_file_b.json")
tweets$text <- sapply(tweets$text, FUN = function(x) gsub("http\\S+\\s*", "", x))

tweet_corpus = Corpus(VectorSource(tweets$text))
tweet_corpus = tm_map(tweet_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
tweet_corpus = tm_map(tweet_corpus, content_transformer(tolower))
tweet_corpus = tm_map(tweet_corpus, removeNumbers)
tweet_corpus = tm_map(tweet_corpus, removeWords, 
                      c("the", "and", "www", "ielts", "IELTS", "twitter","com", "goo" ,
                        stopwords("english")))
tweet_corpus =  tm_map(tweet_corpus, stripWhitespace)
tweet_dtm <- DocumentTermMatrix(tweet_corpus)
tweet_dtm_99 = removeSparseTerms(tweet_dtm, 0.99)

freq = data.frame(sort(colSums(as.matrix(tweet_dtm_99)),
                       decreasing=TRUE))

lsa_tweets <- lsa(tweet_dtm_99)
lsa_tweets <- lsa_tweets['dk']
lsa_tweets <- data.frame(lsa_tweets)

# 3D word Cloud!
# Use the outputted x, y, z coords from running plot_neighbors with size = 0
# as input for iteratively adding dots of decreasing size according to frequency. 
# T-score the word frequencies to get point sizes

xyz <- plot_neighbors("english", 80, tvectors = lsa_tweets, dims = 3, method = "MDS", 
               axes = T, box = T, connect.lines = "all", col = "rainbow", top = T,
               expand = 1.2, size = 0, main = "3-D Word Cloud Representing Semantic Connectedness")

rows <- c(1:80)

ffreq <- (3*(freq[,]-(mean(freq[,])))/sd(freq[,]))+5

for (i in rows){
  plot3d(xyz[i,], add = TRUE, size = ffreq[i])
}
