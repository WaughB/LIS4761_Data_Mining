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
require(tm)

# Load in the data.
tweets <- read_csv("/home/brett/LIS4761_Data_Mining/data/Tweets-2.csv")

# Create a dataframe. 
df1 <- tweets[c("airline_sentiment", "airline", "text")]

##### Cleaning function #####
clean_corpus <- function(corpus) {
  # Remove punctuation.
  corpus <- tm_map(corpus, removePunctuation)
  
  # Transform to lower case.
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Add more stopwords.
  corpus <- tm_map(corpus, removeWords, c("southwestair", "united", "americanair", "usairways", "jetblue", "virginamerica", "flight","can","will","get", "flights", stopwords("en")))
  
  # Strip whitespace.
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

##### Term frequency function #####
top_terms <- function(words) {
  # Create vector of text. 
  words_vs <- VectorSource(words)

  # Create a VCorupus of text. 
  words_vc <- VCorpus(words_vs)

  # Run function on VCorupus.
  words_clean <- clean_corpus(words_vc)

  # Create DocumentTermMatrix
  words_tdm <- TermDocumentMatrix(words_clean)

  # Create matrix for DocumentTermMatrix. 
  words_m <- as.matrix(words_tdm)

  ##### Create the term frequency #####
  # Calculate the rowSums: term_frequency
  term_frequency <- rowSums(words_m)

  # Sort term_frequency in descending order
  term_frequency <- sort(term_frequency, decreasing = TRUE)

  return(term_frequency)
}

##### Create a wordcloud for the positive words #####
# Distinguish the positive entries. 
positive_words <- df1 %>% 
  filter(airline_sentiment == "positive") %>% 
  count(text)

# Call function on positive words. 
positive_corp <- top_terms(positive_words)

# Names of positive_corp. 
positive_term_vec <- names(positive_corp)

# Create a positive wordcloud.
positive_wc <- wordcloud(
  words = positive_term_vec, 
  freq = positive_corp, 
  max.words = 50, 
  colors = c("green","pink","lightblue")
) + title(main = "\n\nPositive Words")

##### Create a wordcloud for the negative words #####
# Distinguish the negative entries. 
negative_words <- tweets %>% 
  filter(airline_sentiment == "negative") %>% 
  count(text)

# Call function on negative words. 
negative_corp <- top_terms(negative_words)

# Names of negative_corp. 
negative_term_vec <- names(negative_corp)

# Create a negative wordcloud.
negative_wc <- wordcloud(
  words = negative_term_vec, 
  freq = negative_corp, 
  max.words = 50, 
  colors = c("red","darkgoldenrod1", "brown")
) + title(main = "\n\nNegative Words")

##### Create a wordcloud for neutral words ##### 
# Distinguish the neutral entries. 
neutral_words <- tweets %>% 
  filter(airline_sentiment == "neutral") %>% 
  count(text)

# Call function on neutral words. 
neutral_corp <- top_terms(neutral_words)

# Names of neutral_corp. 
neutral_term_vec <- names(neutral_corp)

# Create a neutral wordcloud.
neutral_wc <- wordcloud(
  words = neutral_term_vec, 
  freq = neutral_corp, 
  max.words = 50, 
  colors = c("grey80", "darkgoldenrod1", "blue")
) + title(main = "\n\nNeutral Words")


##### Best airline #####
# Dataframe of result from each airline.
df <- tweets %>%  
  group_by(airline, airline_sentiment) %>%
  count(text)

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

# Wordcloud for best airline. 
best_airline_tweets <- tweets %>%  
  filter(airline == "Virgin America", airline_sentiment == "positive") %>%
  count(text)

# Call function on positive best airline words. 
best_airline_corp <- top_terms(best_airline_tweets)

# Names of best_airline_corp. 
best_airline_vec <- names(best_airline_corp)

# Create a best airline wordcloud. 
wordcloud(
  words = best_airline_vec, 
  freq = best_airline_corp, 
  max.words = 50, 
  colors = c("pink", "yellow", "lightgreen")
) + title(main = "\n\nBest Airline")