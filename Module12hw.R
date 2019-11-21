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
library(tm)
library(slam)
hhtweets <- read_csv("hurricane-harvey-tweets.csv")

#________________________________________________________________________________________________________
# Question 1: When did Hervey-related tweets peak in relation to when the hurricane made landfall?
#________________________________________________________________________________________________________

# grouping tweets by datetime and getting a count of tweets for each minute
hhtweets <- group_by(hhtweets, datetime)
twt_cnt <- count(hhtweets)

#creating a plot showing the amount of tweets by time, the vertical line is the time hurricane Harvey hit
ggplot(data = twt_cnt) +
  geom_point(mapping = aes(x = datetime, y = n), alpha = .5) +
  geom_vline(xintercept = twt_cnt$datetime[3026], color = "red") +
  labs(x = "Time", y = "Number of Tweets")

#________________________________________________________________________________________________________
# Question 2: What are the 20 most commonly used words in the Hurricane Harvey tweets?
#________________________________________________________________________________________________________

# creating a list of custom stop words
custom_stop_words <- tibble(word = c("hurricane", "harvey", "hurricaneharvey", "http", "https", "html", "ift.tt", "pic.twitter.com", "twitter.com", "fb.me", "bit.ly", "dlvr.it", "youtube", "youtu.be"))

# unnesting tokens and removing stop words
tweets_un <- hhtweets %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(word) %>%
  count(word)

# gathering the top 20 most frequently used words and creating a bar graph
tweets_un <- tweets_un %>%
  ungroup(word) %>%
  arrange(desc(n))
tweets_un <- top_n(tweets_un, 20)

# plotting a bar graph showing frequency of the top 20 words
ggplot(tweets_un, aes(x= reorder(word, n), y=n)) +
  geom_bar(stat="identity") +
  coord_flip()

#________________________________________________________________________________________________________
# Question 3: What are common words used in tweets that reference refineries?
#________________________________________________________________________________________________________

# creating a string list of the the words of interest
refinery <- c("refinery", "refineries")
refinery_match <- str_c(refinery, collapse = "|")

hhtweets_ref <- hhtweets

# filtering out tweets with "refinery" or "refineries" in them, removing stop words, and creating a word cloud
hhtweets_ref %>%
  filter(str_detect(tweet, refinery_match)) %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(word) %>%
  count(word)%>%
  with(wordcloud(word, n, max.words = 100))

# By creating this word cloud, we are able to see the most frequent words used in tweets that reference
# refineries. Simply by looking at this word cloud, it seems as if those tweeting were more concerned
# with the economic consequences that the storm may have on the refineries. Some stand out words that
# strengthen this argument are: prices, gasoline, oil, markets, business, www.bloomberg.com, and 
# cancellations. While their are some indicators suggesting concern for the environment, it seems as if 
# the majority of these popular words are tied to economics. 


#_________________________________________________________________________________________________________
# Question 4: How did the average sentiment of tweets change from August 17-29, 2019?
#_________________________________________________________________________________________________________

hhtweets_un <- hhtweets

#creates a tibble with unnested tokens and removes stop words
hhtweets_un <- hhtweets_un %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(word) %>%
  count(word)

# afinn is a list of words and their respective sentiments (scale of -5 to 5)
afinn <- get_sentiments("afinn")

hhtweets_word <- hhtweets %>%
  unnest_tokens(word, tweet)

# joins the unnested token tibble with afinn and a tibble including dates and words
hhtweets_un <- hhtweets_un %>%
  inner_join(afinn, by = 'word') %>%
  inner_join(hhtweets_word, by = 'word')

# creating a tibble that includes date, summ of sentiments, number of words per day, and the mean sentiment per day
sentiment <- aggregate(hhtweets_un$value, by=list(date=hhtweets_un$date), FUN=sum, na.rm = T)
sentiment_2 <- count(hhtweets_un)
sentiment_final <- inner_join(sentiment, sentiment_2, by = 'date')
sentiment_final <- mutate(sentiment_final, mean = x/n)

# creating a column chart to display average sentiment per day
ggplot(data = sentiment_final) +
  geom_col(mapping = aes(x = date, y = mean)) +
  scale_x_date(date_breaks = "day", date_labels = "%d") +
  labs(x = "Day in August 2017", y = "Average sentiment")  


















