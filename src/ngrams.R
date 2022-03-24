### N-gram analysis of Family and friends 
source("./src/preprocess_function.R")
source("./src/main.R")

library("dplyr")
library("tidytext")
library("tibble")
library("tidyr")
library("here")
library("igraph")
library("ggraph")

# datadf <- read.csv("./data/text_data.csv")
datadf <- read.csv("./data/text_data.csv")


# datadf <- datadf[c(1:1000),]
text <- as.character(datadf$feedback)
df <- tibble(text=text)


# Calculate word frequency
data_words <- df %>% unnest_tokens(words, text, token = "words") %>% 
  filter(!words %in% stop_words$word) %>%count(words, sort=TRUE)

write.csv(data_words, "words_count.csv")

# Calculate bigram frequency 
data_bigrams <- df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2))  
  
bigrams_freq <- data_bigrams %>% unite(bigram, word1, word2, sep=" ") %>%
  count(bigram, sort=TRUE)

write.csv(data_bigrams,"bigram_count.csv")

# Calculate trigram frequency
data_trigrams <- df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  filter(!is.na(word3)) %>% 
  unite(trigram, word1, word2, word3, sep=" ") %>%
  count(trigram, sort=TRUE)

write.csv(data_trigrams, "trigram_count.csv")



#### Visualise correlation ####
# Table of directed graph 
bigram_graph <- data_bigrams %>%count(word1, word2, sort=TRUE) %>%
  filter(n >= 11) %>% graph_from_data_frame()
bigram_graph

# Visualise directed graph 
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)




### Ngram after processing 
text_cleaned <- clean_text(df)

text_cleaned$Tokens



tokens_skipgrams(text_cleaned$Tokens, n = 3, skip = 0:1, concatenator = " ")
tokens_skipgrams(text_cleaned$Tokens, n = 2, skip = 0:1, concatenator = " ")
count(tokens_skipgrams(text_cleaned$Tokens, n = 2, skip = 0:1, concatenator = " "),
      bigram, sort=TRUE)

data_bigrams <- df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  unite(bigram, word1, word2, sep=" ") %>%
  count(bigram, sort=TRUE)

write.csv(data_bigrams,"bigram_count.csv")

data_trigrams <- df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  filter(!is.na(word3)) %>% 
  unite(trigram, word1, word2, word3, sep=" ") %>%
  count(trigram, sort=TRUE)

write.csv(data_trigrams, "trigram_count.csv")

data_words <- df %>% unnest_tokens(words, text, token = "words") %>% 
  filter(!words %in% stop_words$word) %>%count(words, sort=TRUE)

write.csv(data_words, "words_count.csv")


sessionInfo()
