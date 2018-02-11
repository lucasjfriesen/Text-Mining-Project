# Update custom_stop_words.rds

word <- 

stop_words <- readRDS("custom_stop_words.rds")

stop_words <- rbind(stop_words, c(word, "custom"))

saveRDS(stop_words, "custom_stop_words.rds")