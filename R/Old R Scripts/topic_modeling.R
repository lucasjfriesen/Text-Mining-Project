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
library(jsonlite)
library(LSAfun)

tweets <- fromJSON("/Users/lucasfriesen/Google Drive/R/Main Project/translated_data_B.json")

# Nospam
# tweets <- tweets[!grepl("ielts", tweets$fullname, ignore.case = T, useBytes = T),]
# tweets <- nospam[!grepl("toefl", nospam$fullname, ignore.case = T, useBytes = T),]
# tweets <- nospam[!grepl("english", nospam$fullname, ignore.case = T, useBytes = T),]

# Remove hyperlinks
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
tweets <- tweets %>% 
  mutate(text = str_replace_all(text, replace_reg, ""))

# Create corpus
tweet_corpus = Corpus(VectorSource(tweets$text))

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
tweet_dtm  <- tweet_dtm[rowTotals > 0, ]

# Create wordclouds
par(mfrow=c(1,2))
freq = data.frame(sort(colSums(as.matrix(tweet_dtm)),
                         decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50,
            colors=brewer.pal(1, "Dark2"))

# Create tfidf DTM+cloud
tweet_dtm_tfidf <- DocumentTermMatrix(tweet_corpus, control = list(weighting = weightTfIdf))
tweet_dtm_tfidf = removeSparseTerms(tweet_dtm_tfidf, 0.95)
freq = data.frame(sort(colSums(as.matrix(tweet_dtm_tfidf)), decreasing=TRUE))
# Can fail due to scaling issues. freq is too large a scale?
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

# Further: sentiment analysis: https://rstudio-pubs-static.s3.amazonaws.com/132792_864e3813b0ec47cb95c7e1e2e2ad83e7.html

#
# Beginning topic modeling
# http://tidytextmining.com/topicmodeling.html#latent-dirichlet-allocation
#

# k = number of topics

t_lda <- LDA(tweet_dtm, k = 3, control = list(seed = 1234))

# Word-topic probabilities
t_topics <- tidy(t_lda, matrix = "beta")

t_top_terms <- t_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

t_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") 
  coord_flip()

# Words that show the greatest differences between topics

beta_spread <- t_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# Only shows 2 topics atm.
beta_spread %>%
  group_by(log_ratio < 0) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio, fill = log_ratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("Words with the greatest difference in ?? between topic 2 and topic 1") +
  scale_fill_discrete(name = "", labels = c("Topic 1", "Topic 2"))
