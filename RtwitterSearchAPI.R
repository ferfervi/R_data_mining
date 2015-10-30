#Script that connects to Twitter SEARCH API.

# Furthermore in this example is performed an analysis of the words
# most used in the tweets analyzed.

setwd("~/Development/Rtwitter")
getwd()
dir()

#Import libraries
#install.packages("ROAuth")
#install.packages('base64enc')
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("RWeka")

library(ROAuth)
## Retrieve twitts
library(twitteR)
library(RColorBrewer)#paleta de colores
library(wordcloud)
#library(dplyr)
library(NLP)
library(tm) # analisis de texto
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

#Function to set Twitter connection, read tokens from file.
#Note that previously you need to create a Tweeter APP in the developer
#section and obtain your private keys. I have stored them in a CSV file
# named "TwitterCredentials.csv". This function will read your keys and 
# set the connection with Tweeter SEARCH API. 
#Note that this API does not provide the same funcionality than the STREAMING API.

#ie: the search API does not provide completeness (obtain a representative amount of tweets), 
# and usually one can retrieve tweets created  a few days in advance.
# On the other hand, Tweeter Streaming API captures all tweets matching the 
# query in REAL TIME.
setTwitterConnection = function(filename)
{
  #Twitter connection
  fc=read.csv(filename,colClasses = "character")
  
  Key <- fc$Key
  Secret <- fc$Secret
  accessToken <- fc$AccessToken
  accessSecret <- fc$AccessSecret
  
  setup_twitter_oauth(Key, Secret, access_token= accessToken, access_secret= accessSecret)
}

#Function to clean special characters
cleanText = function(x)
{
  x <- iconv(x,to="latin1")
  #remove URLs
  x <- gsub('http.*\\s*','',x)
  #remove punctiation and catalan punctuation
  x <- gsub("'|,",' ',x)
  x <- gsub("ó|ó",'o',x)
  x <- gsub("í",'i',x)
  x <- gsub("è|é",'e',x)
  x <- gsub("à|á",'a',x)
  x <- gsub("ú",'u',x)
  
  # remove Twitter specifics: at (user referenced) , hashtags and retweets
  x = gsub("@\\w+", "", x)
  x = gsub("#\\w+", "", x)
  x = gsub("RT", "", x)
  return(x)
}

######## INIT #####

#connect to Twitter
setTwitterConnection("TwitterCredentials.csv")

#Query, search Tweets and analysis of WORD MOST USED
set.seed(3505)

#In this example I am looking for the next hashtag,
#which was used in the program ".CAT" of TV3 (29-10-2015)
handle <- '#ForcadellTV3'
txt <- searchTwitter(handle,n=200)


#txt <- searchTwitter(handle,n=3000,since='2015-10-01',until='2015-10-29')
#txt <-closestTrendLocations(lat='41.385064', long='2.173403')
#txt <- getTrends('753692')
#txt <- userTimeline("write_user_name_here")
#txt <- searchTwitter("from:FCBarcelona")

#Obtain tweet text and clean it accoding to the function I wrote above
t <- sapply(txt,function(x) x$getText())
t <- cleanText(t)


#clean and transform creating a Corpus
myWordsToDelete <- c("q")
allWordsToDelete <- c(stopwords("catalan"),stopwords("spanish"),stopwords("english"),myWordsToDelete)

corpusT <- Corpus(VectorSource(t))

corpusT <- tm_map(corpusT, PlainTextDocument)
corpusT <- tm_map(corpusT, content_transformer(tolower))
corpusT <- tm_map(corpusT, removePunctuation)
corpusT <- tm_map(corpusT, removeNumbers)
corpusT <- tm_map(corpusT, removeWords, allWordsToDelete)
corpusT <- tm_map(corpusT, stripWhitespace)

#Term Document Matrix (# word per tweet): Rows->words, columns->Tweets or textFile
tdm = TermDocumentMatrix(corpusT, control=list(wordLengths=c(1,Inf)))
#tdm = TermDocumentMatrix(t_c,control = list(stopwords = myStopwords,stripWhitespace = TRUE,removeNumbers = TRUE,removePunctuation = TRUE, content_transformer(tolower)))

#obtain word (counts) ordered in decreasing order
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing=TRUE) 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#display wordcloud
#wordcloud(dm$word,dm$freq,max.words=70,random.order=FALSE, colors=brewer.pal(6, "Dark2"),rot.per=0.5)
quartz()
wordcloud(dm$word,dm$freq,max.words=70,random.order=FALSE, scale=c(3,.4), colors=brewer.pal(6,"Dark2"))

freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)


########

print("***List words with more frequency:")
print(head(freq, 20))

#termFrequency <- rowSums(as.matrix(tdm))
# load tweets into R
#(n.tweet <- length(tweets))
# convert tweets to a data frame
#tweets.df <- twListToDF(tweets)
#dim(tweets.df)

