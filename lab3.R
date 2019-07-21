# Brett W. 
# LIS4761 - Data Mining
# Lab 3: Word Cloud Chapter Challenge

# Necessary libraries.
require(readr)
require(tm)
require(XML)
require(wordcloud)

# Load in data. 
sample <- read_csv("/home/brett/LIS4761_Data_Mining/data/sample.csv")
data_frame<- do.call('rbind', lapply(sample, as.data.frame))
words.corpus <- Corpus(VectorSource(data_frame))

# Preprocessing.
# Transform to lower case.
words.corpus <- tm_map(words.corpus, content_transformer(tolower))

# Add more stopwords, with "can" and "just" added. 
words.corpus <- tm_map(words.corpus, removeWords, c(stopwords("en"), "can", ""))

# Remove numbers. Could not get this method to work.... Think it is a dependecy issue. 
#words.corpus <- removeNumbers(words.corpus)

# Remove punctuation.
words.corpus <- tm_map(words.corpus, removePunctuation)

# Wordcloud part.
tdm <- TermDocumentMatrix(words.corpus)
tdm

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)

# Create wordcloud.
cloudFrame <- data.frame(word = names(wordCounts), freq=wordCounts)

wordcloud(cloudFrame$word, cloudFrame$freq)

wordcloud(names(wordCounts), wordCounts, min.freq=2, max.words=50, rot.per=0.35, 
          colors=brewer.pal(5, "Accent"))
