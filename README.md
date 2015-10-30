# R_data_mining

In this repository I will commit some functions that I had stored locally and can be handy when performing data analysis with Twitter (for example).

### section 1: SEARCH API

Script to connect to Tweeter Search API, retrieve twets and perform some text cleaning / analysis displaying a wordcloud with the words more used in the tweets.

##### Wordcloud of the example provided, words most used in the tweets in the program ".CAT" of TV3 (Catalan TV 2015-10-29).  
![alt text](https://github.com/ferfervi/R_data_mining/blob/master/textAnalysisForcadell.jpeg  "WORDCLOUD")

```
##### Script that connects to Twitter SEARCH API.

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


```


### section 2: STREAMING API

```
setwd("~/Development/Rtwitter")
getwd()
dir()

library(streamR)
library(twitteR)
library(stringr)
library(XML)
library(dismo)

# Note, I have written this function for setting the connection to Twitter Streaming API,
# which requires reading from a CSV file where we have stored previously our API parameters
# -consumerKey
# -consumerSecret
# -accessToken
# -accessSecret
setTwitterConnection = function(filename)
{
  # Part 1: Twitter connection, read my private application keys (first create twitter app in developer section)
  fc=read.csv(filename,colClasses = "character")
  
  Key <- fc$Key
  Secret <- fc$Secret
  accessToken <- fc$AccessToken
  accessSecret <- fc$AccessSecret
  
  # PART 2: Declare Twitter API Credentials & Create Handshake
  library(ROAuth)
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  
  my_oauth <- OAuthFactory$new(consumerKey = Key,
                               consumerSecret = Secret,
                               requestURL = requestURL,
                               accessURL = accessURL,
                               authURL = authURL)
  
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

  # PART 2: Save the my_oauth data to an .Rdata file
  save(my_oauth, file = "my_oauth.Rdata")

}



######## INIT - SET CONNECTION #####

#set new connection to Twitter (only required the first time). Next times you can access 
#loading "my_oauth.Rdata". Uncomment the next line for setting up the connection. Do not
#forget to comment it again after having stored the connection settings in the previous file.

#OPTION A) Only if we haven´t set the connection previously
#setTwitterConnection("TwitterCredentials.csv")

#OPTION B) Next times load connection settings from file
load("my_oauth.Rdata")

######################

# C) Capture Twitts using the Sreaming API
#Select wich case to RUN:
switch(1, 
       {# C.1) Streaming, parsing tweets case 1.
         
         #Empty previous tweets
         #if(file.exists("tweets.json")) file.remove("tweets.json")
         
         filterStream(file.name = "tweets.json", # Save tweets in a json file
                      track = c("ForcadellTV3","#ForcadellTv3"), # Collect tweets 
                      timeout = 3600, # Keep connection alive for X seconds
                      oauth = my_oauth) # Use my_oauth file as the OAuth credentials
         tweets.df <- parseTweets("tweets.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
       }, 
       
      {# C.2) Streaming, parsing tweets case 2. 
        
        #Empty previous tweets
        if(file.exists("tweets.json")) file.remove("tweets.json")
        
        filterStream(file.name = "tweets.json", # Save tweets in a json file
                     track = c("FCBarcelona", "futbol"), # Collect tweets 
                     language = "es",
                     timeout = 30, # Keep connection alive for 30 seconds
                     oauth = my_oauth) # Use my_oauth file as the OAuth credentials
        tweets.df <- parseTweets("tweets.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
      }
              
       )

```

