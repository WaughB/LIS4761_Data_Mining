# Brett Waugh
# LIS4761 - Data and Text Mining
# Final Project - Flight Review

# Necessary libraries.
require(readr)
require(tidyverse)
require(textverse)
require(wordcloud)
require(dplyr)
require(tidyr)
require(tidytext)

# Load in the data.
tweets <- read_csv("/home/brett/LIS4761_Data_Mining/data/Tweets-2.csv")

##### Create a wordcloud for the positive words #####
# Distinguish the positive entries. 
positive_words <- tweets %>% 
  filter(airline_sentiment == "positive") %>% 
  count(text)

# Create a positive wordcloud, with green words. 
wordcloud(
  words = positive_words$text, 
  freq = positive_words$n, 
  max.words = 3, 
  colors = "green"
)

##### Create a wordcloud for the negative words #####
# Distinguish the negative entries. 
negative_words <- tweets %>% 
  filter(airline_sentiment == "negative") %>% 
  count(text)

# Create a negative wordcloud, with red words. 
wordcloud(
  words = negative_words$text, 
  freq = negative_words$n, 
  max.words = 25, 
  colors = "red"
)

##### Create a wordcloud for neutral words ##### 
# Distinguish the neutral entries. 
neutral_words <- tweets %>% 
  filter(airline_sentiment == "neutral") %>% 
  count(text)

# Create a neutral wordcloud, with blue text. 
wordcloud(
  words = neutral_words$text, 
  freq = neutral_words$n, 
  max.words = 50, 
  colors = "blue"
)

##### Best airline??? #####
# Dataframe of result from each airline.
df <- tweets %>%  
  + group_by(airline, airline_sentiment) %>%
  + count()

# Barplot of airline sentiment. 
ggplot(df, aes(x=airline, y = n, fill = airline_sentiment)) + 
  geom_col() +
  labs(
    title = "Sentiment by Airline",
    x = "Airline", 
    y = "Number of Reviews", 
    caption = "Based on data from: https://usflearn.instructure.com/courses/1402294/files/79944018/download?download_frd=1"
  )

# Normalized barplot of airline sentiment. 
ggplot(df, aes(x=airline, y = n, fill = airline_sentiment)) + 
  geom_col(position = "fill") + 
  labs(
    title = "Sentiment by Airline",
    x = "Airline", 
    y = "Number of Reviews", 
    caption = "Based on data from: https://usflearn.instructure.com/courses/1402294/files/79944018/download?download_frd=1"
  )