#Module 12 Homework

rm(list = ls())

install.packages(tidytext)
install.packages(wordcloud)
install.packages(textdata)

library(tidyverse)
library(tidytext)
library(wordcloud)
library(textdata)
library(lubridate)
hhtweets <- read_csv("hurricane-harvey-tweets.csv")


# Question 1: When did Hervey-related tweets peak in relation to when the hurricane made landfall?

hhtweets <- group_by(hhtweets, datetime)
hhtweets
twt_cnt <- count(hhtweets)
twt_cnt
hhtweets

ggplot(data = twt_cnt) +
  geom_point(mapping = aes(x = datetime, y = n), alpha = .5) +
  geom_vline(xintercept = as.numeric("2017-08-26 03:00:00"), color = "red") + #########################################
  labs(x = "Time", y = "Number of Tweets")

# Question 2: What are the 20 most commonly used words in the Hurricane Harvey tweets?

custom_stop_words <- tibble(word = c("hurricane", "harvey", "hurricaneharvey", "http", "https", "html", "ift.tt", "pic.twitter.com", "twitter.com", "fb.me", "bit.ly", "dlvr.it", "youtube", "youtu.be"))

tweets_un <- hhtweets %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(word) %>%
  count(word)

tweets_un %>%
  arrange(desc(n)) %>%
  top_n(20, n) %>%
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat = "identity") ################################################################################


# Question 3: What are common words used in tweets that reference refineries?

refinery <- c("refinery", "refineries")
refinery_match <- str_c(refinery, collapse = "|")
#refinery_sub <- str_subset(hhtweets$tweet, refinery_match)

hhtweets_ref <- hhtweets %>%
  filter(str_detect(tweet, refinery_match)) %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(word) %>%
  count(word) %>%
  #wordcloud(word, max.words = 100)

hhtweets
#refinery_wc
#tail(ref)


# Question 4: How did the average sentiment of tweets change from August 17-29, 2019?

hhtweets_un <- hhtweets

hhtweets_un <- hhtweets_un %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(word) %>%
  count(word)

hhtweets
hhtweets_un

afinn <- get_sentiments("afinn")
hhtweets_un %>%
  inner_join(afinn, by = 'word')




















