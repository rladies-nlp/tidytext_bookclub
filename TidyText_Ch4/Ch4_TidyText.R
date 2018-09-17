## Chapter 4 of Tidy Text
## R-Ladies Chicago NLP Study Group 

library(tidytext)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(readtext)

## Using scripts of the first three Star Wars Films to do some bigram analyses!

## We have a folder with three .txt files (these are the scripts for each film)
## Read these files into R with readtext package
# Create list of files in a directory
DATA_DIR <- system.file("starwars/", package = "readtext")

# Make a table with doc_id as each row, text as the second column 
text <- readtext(paste0(DATA_DIR, "starwars/*"))

## Unnest tokens into 2 word utterances
by_bigram <- text %>% 
  unnest_tokens(bigram, text, token="ngrams", n=2)

by_bigram # View tibble

## Make each word in the bigram its own column
bigrams_separated <- by_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated # View tibble

## Let's filter out stop words and other select words
data("stop_words")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1 %in% c('character','dialogue')) %>% 
  filter(!word2 %in% c('character','dialogue')) %>%
  filter(!str_detect(word1, "[0-9]")) %>% 
  filter(!str_detect(word2, "[0-9]"))

bigrams_filtered <- as_tibble(bigrams_filtered)

## See counts of each word
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

### Sentiment Analysis
## Use AFINN lexicon for sentiment analysis
## This lexicon gives a numeric sentiment score for each word, with positive or negative numbers indicating the direction of the sentiment.
AFINN <- get_sentiments("afinn")
 
## Get words that are preceded by "not"
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words # View tibble

## Which "not_words" had the largest contribution to sentiment and in what direction?
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

## How about other negation words?
negation_words <- c("not", "no", "never", "without") # Negation words that were used in the book

## Get words preceded by negation words
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words # View tibble

# Visualize the contribution to sentiment analysis by each individual negation word
negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, nrow=1) +
  xlab("") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()
