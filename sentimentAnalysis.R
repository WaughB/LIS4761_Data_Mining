# Brett W. 
# LIS4761 - Data Mining
# Sentiment Analysis

# Necessary libraries. 
require(readr)
require(tm)
require(XML)
require(ggplot2)

# Read in list of positive and negative words. 
pos <- "/home/brett/LIS4761_Data_Mining/data/opinion-lexicon-English/positive-words.txt"
neg <- "/home/brett/LIS4761_Data_Mining/data/opinion-lexicon-English/negative-words.txt"

p <- scan(pos, character(0),sep = "\n")
n <- scan(neg, character(0),sep = "\n")

# Load in the speech file.
# Read and parse HTML file.
doc.html = htmlTreeParse('http://www.analytictech.com/mb021/mlk.htm', useInternal = TRUE)

# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))

# Replace all \n by spaces
doc.text = gsub('\\n', ' ', doc.text)

# Replace all \r by spaces
doc.text = gsub('\\r', ' ', doc.text)

# Create the corpus. 
words.vec <- VectorSource(doc.text)
words.corpus <- Corpus(words.vec)
words.corpus

# Create the term matrix.
tdm <- TermDocumentMatrix(words.corpus)
tdm

# Create the matrix with counts. 
m <- as.matrix(tdm)
wordCounts <- rowSums(m)

# Determine what percentage of the speech was positive. 
totalWords <- sum(wordCounts)
words <- names(wordCounts)
matched <- match(words, p, nomatch = 0) 

mCounts <- wordCounts[which(matched != 0)]
length(mCounts)
mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos

# Determine which percentage of the speech was negative. 
matched <- match(words, n, nomatch = 0)
nCounts <- wordCounts[which(matched != 0)]
nNeg <- sum(nCounts)
nWords <- names(nCounts)
nNeg
length(nCounts)

# Final percentages. 
totalWords <- length(words)
ratioPos <- nPos/totalWords
ratioPos
ratioNeg <- nNeg/totalWords
ratioNeg
totalRatio <- nPos/nNeg
totalRatio

# Visualization of positive and negative words by section of the speech. 
firstPart <- wordCounts[0:83]
secondPart <- wordCounts[84:166]
thirdPart <- wordCounts[167:249]
fourthPart <- wordCounts[250:332]

# First section of the speech. 
firstWords <- names(firstPart)
matched <- match(firstWords, p, nomatch = 0) 

m1Counts <- firstPart[which(matched != 0)]
length(m1Counts)
m1Words <- names(m1Counts)
n1Pos <- sum(m1Counts)
n1Pos

matched <- match(firstWords, n, nomatch = 0)
n1Counts <- secondPart[which(matched != 0)]
n1Neg <- sum(n1Counts)
n1Words <- names(n1Counts)
n1Neg

# Second part of the speech. 
secondWords <- names(secondPart)
matched <- match(secondWords, p, nomatch = 0) 

m2Counts <- secondPart[which(matched != 0)]
length(m2Counts)
m2Words <- names(m2Counts)
n2Pos <- sum(m2Counts)
n2Pos

matched <- match(secondWords, n, nomatch = 0)
n2Counts <- secondPart[which(matched != 0)]
n2Neg <- sum(n2Counts)
n2Words <- names(n2Counts)
n2Neg

# Third part of the speech. 
thirdWords <- names(thirdPart)
matched <- match(thirdWords, p, nomatch = 0) 

m3Counts <- thirdPart[which(matched != 0)]
length(m3Counts)
m3Words <- names(m3Counts)
n3Pos <- sum(m3Counts)
n3Pos

matched <- match(thirdWords, n, nomatch = 0)
n3Counts <- thirdPart[which(matched != 0)]
n3Neg <- sum(n3Counts)
n3Words <- names(n3Counts)
n3Neg

# Fourth part of the speech. 
fourthWords <- names(fourthPart)
matched <- match(fourthWords, p, nomatch = 0) 

m4Counts <- fourthPart[which(matched != 0)]
length(m4Counts)
m4Words <- names(m4Counts)
n4Pos <- sum(m4Counts)
n4Pos

matched <- match(fourthWords, n, nomatch = 0)
n4Counts <- fourthPart[which(matched != 0)]
n4Neg <- sum(n4Counts)
n4Words <- names(n4Counts)
n4Neg

# Calculate ratios for each section of the speech. 
ratioPos1 <- n1Pos/totalWords
ratioNeg1 <- n1Neg/totalWords
ratio1 <- ratioPos1 / ratioNeg1
ratio1

ratioPos2 <- n2Pos/totalWords
ratioNeg2 <- n2Neg/totalWords
ratio2 <- ratioPos2 / ratioNeg2

ratioPos3 <- n3Pos/totalWords
ratioNeg3 <- n3Neg/totalWords
ratio3 <- ratioPos3 / ratioNeg3

ratioPos4 <- n4Pos/totalWords
ratioNeg4 <- n4Neg/totalWords
ratio4 <- ratioPos4 / ratioNeg4

# Dataframe of ratios. 
ratios <- c(ratio1, ratio2, ratio3, ratio4)

# Visualization of positive to negative semantics during the speech. 
barplot(ratios, main = "Semantics of Dr. King Speech", sub = "based on speech from: http://www.analytictech.com/mb021/mlk.htm", xlab = "Section of the Speech", ylab = "Positive to Negeative Ratio")