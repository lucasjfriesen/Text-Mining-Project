# timescale == "annual", "monthly"
# cloudtype == "raw", "DTF", "TfIdf"

wc_series <- function(data, timescale = "yyyy", cloudtype = "raw", clean = TRUE) {
  pal <- (pal_jco("default")(10))
  
  if (timescale == "monthly") {
    timescale <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10','11','12')
    period_ <- "mm"
  } else { 
    timescale <- c('2007', '2008', '2009', '2010','2011','2012', '2013', '2014', '2015', '2016', '2017')
    period_ <- "yyyy"
  }
  
  for (x in timescale) {
    
    current <- paste("period-", x, sep = "")
    assign(current, subset(data, Format(as.Date(data$timestamp), fmt = period_)==x))
    current <- get(current)
    
    if (nrow(current) != 0) {
      if (clean == TRUE) {
      cleaned_tweets <- current %>%
        select(timestamp, text) %>%
        mutate(text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", text)) %>%
        mutate(document = row_number()) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)
      
        freq_tweets <- cleaned_tweets %>% 
          count(document, word, sort = TRUE)
      } else {
        cleaned_tweets <- current %>%
          select(timestamp, text, lang) %>%
          mutate(document = row_number()) %>%
          unnest_tokens(word, text)
        
          freq_tweets <- cleaned_tweets %>% 
            count(document, word, sort = TRUE) 
      }
      if (cloudtype != "raw") {
        if (cloudtype == "TfIdf") {
          weight_ <- tm::weightTfIdf
        } else {
          weight_ <- tm::weightTf
        }
        # Create DTM using TidyText, remove sparse terms. Play with the values here
        cloud_tweets <- freq_tweets %>% 
          cast_dtm(document, word, n, weighting = weight_) %>%
          removeSparseTerms(0.9999)
        cloud_freq <- data.frame(sort(colSums(as.matrix(cloud_tweets)),
                                      decreasing=TRUE))
        cloud_df <- rownames(cloud_freq)
      } else {
        cloud_df <- freq_tweets$word
        cloud_freq <- data.frame(freq_tweets$n)
      }
      # create word cloud
      wordcloud(cloud_df, cloud_freq[,1], max.words=50, colors=pal, random.order = F, rot.per = 0,
                scale = c(4, .7))
      title(main = cloudtype, sub = x)
    }
  }
}
