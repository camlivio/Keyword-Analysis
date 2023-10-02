#### CNN news data set from Kaggle
## Camila Lívio
## Fall 2023
## Demo for Dr. Frans Weiser's students

library(tidytext)
library(tidyverse)
library(dplyr)
library(textdata)
library(ggplot2)

range(CNN_Articels_clean$`Date published`)
CNN_Articels_clean -> cnn2

cnn_tknzed <- cnn2 %>%
  unnest_tokens(word, `Article text`)

cnn_tknzed %>%
  count(word, sort = TRUE)

tidy_cnn <- cnn_tknzed %>%
  anti_join(stop_words)

tidy_cnn %>%
  count(word, sort = TRUE)

tidy_cnn %>%
  count(word, sort = TRUE) %>%
  top_n(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL, title="Frequent words in CNN data")

#### on to bigrams
cnn_bigrams2 <- cnn2 %>%
  unnest_tokens(bigram, `Article text`, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))
cnn_bigrams2

cnn_bigrams2 %>%
  count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated2 <- cnn_bigrams2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered2 <- bigrams_separated2 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigrams counts:
bigram_counts2 <- bigrams_filtered2 %>%
  count(word1, word2, sort = TRUE)

bigram_counts2

bigrams_filtered2 %>%
  filter(word2 == "film") %>%
  count(word1, sort = TRUE) %>%
  print(n=20)

bigrams_filtered %>%
  filter(word2 == "adaptation") %>%
  count(word1, sort = TRUE) %>%
  print(n=50)

bigrams_separated %>%
  filter(word2 == "literature") %>%
  count(word1, sort = TRUE) %>%
  print(n=50)

library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)

cnn_kwic2 <- kwic(cnn2$`Article text`, pattern = "movie", case_insensitive = TRUE)
nrow(cnn_kwic)
head(cnn_kwic2, 25)

library(tidytext)
library(textdata)
library(tidytext)
get_sentiments("nrc")

textdata::lexicon_nrc(delete = TRUE)
textdata::lexicon_nrc()

film_words <- bigrams_separated2 %>%
  filter(word2 == "film") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

film_words


library(igraph)

# original counts
bigram_counts

bigram_graph2 <- bigram_counts2 %>%
  filter(n > 150) %>%
  graph_from_data_frame()
bigram_graph

##### I used the code above from Julia Silge and applied it to my word2==film bigram counts to plot a network
bigram_graph2 <- bigrams_separated2 %>%
  anti_join(stop_words) %>%
  filter(word2 == "film") %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 5)
bigram_graph2
library(ggraph)
set.seed(2017)

ggraph(bigram_graph2, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph2, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

