library(tm)
library(SnowballC)
library(wordcloud)
library(readxl)
library(tidytext)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(topicmodels)
library(tidyr)
library(DescTools)
library(jsonlite)

tweets <- fromJSON("Collected data/merged_file_b.json")

# CHOOSE ONE

monthly <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10','11','12')
periodf <- 'mm'

annual <- c('2007', '2008', '2009', '2010','2011','2012', '2013', '2014', '2015', '2016', '2017')
periodf <- 'yyyy'

for (period in annual){
  nam <- paste("period-", period, sep = "")
  assign(nam, subset(tweets, Format(as.Date(tweets$timestamp), fmt = periodf)==period))
  nam <- get(nam)
  # Nospam
  # tweets <- tweets[!grepl("ielts", tweets$fullname, ignore.case = T, useBytes = T),]
  # tweets <- nospam[!grepl("toefl", nospam$fullname, ignore.case = T, useBytes = T),]
  # tweets <- nospam[!grepl("english", nospam$fullname, ignore.case = T, useBytes = T),]
  
  # Remove hyperlinks
  replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
  nam <- nam %>% 
    mutate(text = str_replace_all(text, replace_reg, ""))
  
  # Create corpus
  tweet_corpus = Corpus(VectorSource(nam$text))
  
  # Clean non UTF-8 chars
  tweet_corpus = tm_map(tweet_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  
  # Clean the corpus
  tweet_corpus = tm_map(tweet_corpus, content_transformer(tolower))
  tweet_corpus = tm_map(tweet_corpus, removeNumbers)
  tweet_corpus = tm_map(tweet_corpus, removeWords, 
                        c("the", "and", "www", "ielts", "IELTS", "twitter","com", "goo" ,
                          stopwords("english")))
  tweet_corpus =  tm_map(tweet_corpus, stripWhitespace)
  
  # Create DTM
  tweet_dtm <- DocumentTermMatrix(tweet_corpus)
  tweet_dtm = removeSparseTerms(tweet_dtm, 0.99)
  
  # Clean DTM by removing rows with no terms
  rowTotals <- apply(tweet_dtm , 1, sum)
  tweet_dtm  <- tweet_dtm[rowTotals> 0, ]
  
  # Create wordclouds
  par(mfrow=c(1,2))
  freq = data.frame(sort(colSums(as.matrix(tweet_dtm)),
                         decreasing=TRUE))
  wordcloud(rownames(freq), freq[,1], max.words=50,
            colors=brewer.pal(1, "Dark2"))
  title(main = c("DTM: ", period), line = -2)
  
  # Create tfidf DTM+cloud
  tweet_dtm_tfidf <- DocumentTermMatrix(tweet_corpus, control = list(weighting = weightTfIdf))
  tweet_dtm_tfidf = removeSparseTerms(tweet_dtm_tfidf, 0.95)
  freq = data.frame(sort(colSums(as.matrix(tweet_dtm_tfidf)), decreasing=TRUE))
  # Can fail due to scaling issues. freq is too large a scale?
  wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))
  title(main = c("TFIDF: ", period), line = -2)
}