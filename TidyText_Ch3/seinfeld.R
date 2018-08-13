## Chapter 3 of Tidy Text
#  tf-idf using Seinfeld Scripts
#  Written for R-Ladies Chicago NLP Study Group 20180813

library(dplyr)
library(tidytext)
library(ggplot2)

# Load Data
data_fr_file <- as_tibble(read.csv("scripts.csv",stringsAsFactors = FALSE))
seinfeld <- data_fr_file

seinfeld # View data

# Unnest so each line is a word
seinfeld_unnest <- seinfeld %>% 
  unnest_tokens(word, Dialogue)

seinfeld_unnest

# Filter out stop words
data(stop_words) # get stop words from TidyText package
seinfeld_unnest <- seinfeld_unnest %>% # Filter out stop words
  anti_join(stop_words) 

# Word Count by Character
seinfeld_char_wc <- seinfeld_unnest %>%
  count(Character, word, sort = TRUE) 

seinfeld_char_wc 

# Word Count by Season
seinfeld_season_wc <- seinfeld_unnest %>%
  count(Season, word, sort = TRUE) 

seinfeld_season_wc

# Term frequency of words by season 
total_words <- seinfeld_season_wc %>% group_by(Season) %>% summarize(total = sum(n))
seinfeld_seasons <- left_join(seinfeld_season_wc, total_words)
seinfeld_seasons

# Graph Term Frequency Distribution
ggplot(seinfeld_seasons, aes(n/total, fill = as.factor(Season))) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~Season, ncol = 5, scales = "free_y")

# Find out tf_idf
seinfeld_seasons_calc <- seinfeld_seasons %>%
  bind_tf_idf(word, Season, n)

seinfeld_seasons_calc # View data 

# Arrange by largest tf_idf value first
seinfeld_seasons_calc %>%
  arrange(desc(tf_idf,n))

# Plot tf_idf of words by Season of Seinfeld
seinfeld_seasons_calc %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Season) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = as.factor(Season))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Season, ncol = 4, scales = "free") +
  coord_flip()
