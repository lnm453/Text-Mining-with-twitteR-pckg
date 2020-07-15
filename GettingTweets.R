# Load the necessary packages
library(twitteR)
library(bitops)
library(RCurl)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(sentiment)

# OAuth settings:
library(httr)

## Twitter Data mining
# Go to https://apps.twitter.com
api_key <- 'A5ymhi5vqTSCCgU1ztxtHnMq6'
api_secret <- 'PKpk0FyqIiwzyYGuAMJ9I10AadG1VZ3D00ZGUmG16z3HRkqroi'
access_token <- 	'810149683024822277-82p5teHlf4aCXc1WOAPhKsSUCCZ1riF'
access_token_secret <- 		'Mj8ufFM6LOKDDx6m3TeMi7gOIN5YmuPeDznLujrI4mEcZ'

#install.packages("twitteR")
library(twitteR)
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# getting tweets
tweets <- searchTwitter('#dillards', n=500, lang= 'en')
#apple <- searchTwitter('$aapl', n=500, lang= 'en')

tweetsdf <- twListToDF(tweets)
write.csv(tweetsdf, file = "~/Google Drive/Big Data/Feb 28/dillards_tweet.csv",
          row.names = F)
head(dillards)

# Trend locations
trend <- availableTrendLocations()
head(trend)

# Getting trends
world <- getTrends(1)
world

# User timeline
t <- getUser('')
userTimeline(t, n=2)

# loading the data
options(header=FALSE, stringsAsFactors = FALSE,FileEncoding="latin1")
dillards <- read.csv("~/Google Drive/Documents/Spring 2017/Big Data/Feb 28/dillards_tweet.csv")
require(NLP)
library(tm)
library(SnowballC)

# combining all tweets together
dillards_text <- paste(dillards$text, collapse = " ")
#corpus <- iconv(dillards_text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(dillards_text))

# cleaning corpus
corpus <- tm_map(corpus, content_transformer(tolower),lazy=TRUE)
corpus <- tm_map(corpus, removePunctuation,lazy=TRUE)
corpus <- tm_map(corpus, removeNumbers,lazy=TRUE)
cleanset <- tm_map(corpus, removeWords, stopwords("english"),lazy=TRUE)
#remove URL
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
cleanset <- tm_map(cleanset, content_transformer(removeURL), lazy=TRUE)
#remove more stop words
cleanset <- tm_map(cleanset, removeWords, "via")
cleanset <- tm_map(cleanset, gsub, pattern="beads", replacement="bead")
cleanset <- tm_map(cleanset, stripWhitespace,lazy=TRUE)
cleanset <- tm_map(cleanset, PlainTextDocument,lazy=TRUE)
inspect(cleanset)

# Making a document-term matrix
cleanset <- tm_map(cleanset, PlainTextDocument,lazy=TRUE)
tdm <- TermDocumentMatrix(cleanset, control=list(minWordLength=c(1, Inf))) # or minWordLength=c(20)

#dtm2 <- as.matrix(dtm)

# Finding most frequent terms
freq <- colSums(dtm2)
freq <- sort(frequency, decreasing = T)
# or 
findFreqTerms(tdm, lowfreq=3)

# bar plot
t <- rowSums(as.matrix(tdm))
t <- subset(t, t>2)   #only use a subset with larger frequencies
barplot(t, col = rainbow(40), las=2, ylim = c(0,10)) 

#WORD CLOUD
#install.packages("wordcloud")
#install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)
m<- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
#set.seed(205) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )

wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=2, random.order=F,
          max.words = 100,
          colors=brewer.pal(6, "Dark2"),
          #rot.per = 0.2       #to rotate
          scale = c(3, 1))
wordcloud(m)







