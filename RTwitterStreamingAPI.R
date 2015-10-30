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


# C) Capture Twitts using the Sreaming API
captureTweetsRealTime = function(case)
{
      #Select wich case to RUN:
      switch(case, 
          {# C.1) Streaming, parsing tweets case 1.
            
            #Empty previous tweets
            #if(file.exists("tweets.json")) file.remove("tweets.json")
            
            filterStream(file.name = "tweets.json", # Save tweets in a json file
                         track = c("ForcadellTV3","#ForcadellTv3"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                         timeout = 3600, # Keep connection alive for 60 seconds
                         oauth = my_oauth) # Use my_oauth file as the OAuth credentials
            tweets.df <- parseTweets("tweets.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
          }, 
          
          {# C.2) Streaming, parsing tweets case 2. 
            
            #Empty previous tweets
            if(file.exists("tweets.json")) file.remove("tweets.json")
            
            filterStream(file.name = "tweets.json", # Save tweets in a json file
                         track = c("FCBarcelona", "futbol"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                         language = "es",
                         timeout = 30, # Keep connection alive for 60 seconds
                         oauth = my_oauth) # Use my_oauth file as the OAuth credentials
            tweets.df <- parseTweets("tweets.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
          }
          
        )
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

# C) Capture Twitts using the Sreaming API. Uncomment to start capturing (blocking-real time)
# captureTweetsRealTime(1)

# D) Process tweets. Analysis.

# D.1) Plot location from where were made the tweets. Note that this is not exact since we are trying
# to read the field where the user wrote that he/she is living and later trying to figure out the coordenates.
library(ggmap)
library(ggplot2)

tweetsToAnalyze <- parseTweets("tweets.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
locatedUsers <- !is.na(tweetsToAnalyze$location)
locations <- geocode(tweetsToAnalyze$location[locatedUsers])  # Use amazing API to guess


# Create a data frame with the locations (coordenates) to display in the map
#d <- data.frame(lat=c(50.659631, 50.607213, 50.608129),lon=c(3.09319, 3.011473, 3.031529))
df_locations <- data.frame(lat=locations$lat,lon=locations$lon)

#Select map zone to display, size and paint points in the coordenates
myMap <- get_map("Catalonia", zoom=8)
p <- ggmap(myMap)
p + geom_point(data=d, aes(x=lon, y=lat), color="red", size=6, alpha=0.3)
