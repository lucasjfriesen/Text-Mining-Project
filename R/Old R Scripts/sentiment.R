library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(jsonlite)
library(DescTools)

tweets <- fromJSON("/Users/lucasfriesen/Projects/Tweets/merged_file.json")

# original_books <- austen_books() %>%
#  group_by(book) %>%
#  mutate(linenumber = row_number(),
#         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
#                                                 ignore_case = TRUE)))) %>%
#  ungroup()
# 
# original_books

original_tweets <- tweets %>% 
  mutate(linenumber = row_number(), year = StrTrunc(timestamp, maxlen = 4))

# tidy_books <- original_books %>%
#   unnest_tokens(word, text)

tidy_tweets <- original_tweets %>%
  unnest_tokens(word, text)

# tidy_books

data("stop_words")
tidy_books <- tidy_books %>%
  anti_join(stop_words)

# tidy_books %>%
#   count(word, sort = TRUE)

tidy_tweets %>%
  count(word, sort = TRUE)

library(tidyr)
bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

# janeaustensentiment <- tidy_books %>%
#   inner_join(bing) %>%
#   count(book, index = linenumber %/% 80, sentiment) %>%
#   spread(sentiment, n, fill = 0) %>%
#   mutate(sentiment = positive - negative)

tweetsent <- tidy_tweets %>%
  inner_join(bing) %>% 
  count(year, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

tweetsent

library(ggplot2)
library(viridis)
library(widyr)
# ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   facet_wrap(~book, ncol = 3, scales = "free_x") +
#   theme_minimal(base_size = 13) +
#   labs(title = "Sentiment in Jane Austen's Novels",
#        y = "Sentiment") +
#   scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
#   scale_x_discrete(expand=c(0.02,0)) +
#   theme(strip.text=element_text(hjust=0)) +
#   theme(strip.text = element_text(face = "italic")) +
#   theme(axis.title.x=element_blank()) +
#   theme(axis.ticks.x=element_blank()) +
#   theme(axis.text.x=element_blank())

ggplot(tweetsent, aes(index, sentiment, fill = year)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~year, ncol = 3, scales = "free_x") +
  theme_minimal(base_size = 13) +
  labs(title = "Sentiment tweets by year",
       y = "Sentiment") +
  scale_fill_viridis(end = 0.75, discrete=TRUE, direction = -1) +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
 
# austen_sentences <- austen_books() %>% 
#   group_by(book) %>% 
#   unnest_tokens(sentence, text, token = "sentences") %>% 
#   ungroup()

tidy_tweets_sentence <- original_tweets %>%
  unnest_tokens(sentence, text, token = "sentences")

tidy_tweets_sentence <- tidy_tweets_sentence %>% 
  mutate(linenumber = row_number(), year = StrTrunc(timestamp, maxlen = 4))

bingnegative <- sentiments %>%
  filter(lexicon == "bing", sentiment == "negative")

wordcounts <- tidy_tweets_sentence %>%
  group_by(year) %>%
  summarize(words = n())

# tidy_books %>%
#   semi_join(bingnegative) %>%
#   group_by(book, chapter) %>%
#   summarize(negativewords = n()) %>%
#   left_join(wordcounts, by = c("book", "chapter")) %>%
#   mutate(ratio = negativewords/words) %>%
#   filter(chapter != 0) %>%
#   top_n(1)

tidy_tweets %>%
  semi_join(bingnegative) %>%
  group_by(year) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("year")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(year != 0) %>%
  top_n(1)

# pride_prejudice_words <- tidy_books %>%
#   filter(book == "Pride & Prejudice")
# word_cooccurences <- pride_prejudice_words %>%
#   pairwise_count(word, linenumber, sort = TRUE)
# word_cooccurences

words_2009 <- tidy_tweets %>%
  filter(year == "2009...")
word_cooccurences <- tidy_tweets %>%
  pairwise_count(word, linenumber, sort = TRUE)
word_cooccurences

library(igraph)
library(ggraph)

set.seed(2000)
word_cooccurences %>%
  filter(n >= 350) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8) +
  ggtitle(expression(paste("Word Network in 2009 tweets")) +
  theme_void())