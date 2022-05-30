#Install package from Twitter
install.packages("twitteR")
#Creating a live connection with twitter
library(twitteR)
api_key<-'IbtnD7jelY71oB2ZOW0MogI2Q'
api_secret<-'enFOEdlDbx9GgXrbH8s1mOyOAj9zVrZ6qtMP4nb9mkoeqNfSel'
access_token<-'987964433711300614-TTYwLNT79dbdfxMlby5LPOyiYIzkCiS'
access_token_secret<-'loBbDmoZeylVOlw9iJWC6JfhDBCcTl4Xd4ZzfCDeLM5lZ'
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
#Pulling Tweets data
tweets<-searchTwitter("CSKvSRH",n=5000,since = "2020-07-01",lang = "en")
#count the number of tweets
n.tweets<-length(tweets)
#convert tweets to data frame
tweets.df<-twListToDF(tweets)
View(tweets.df)


#Cleaning the tweets
library(tm)
srh<-Corpus(VectorSource(tweets.df$text))
srh<-tm_map(srh,removeWords,stopwords())
mystopword<-c("<U+25BA>",".","<",">","+")
srh<-tm_map(srh,removeWords,mystopword)
  
#remove url
remove_url<-function(x)gsub("http[^[:space:]]","",x)

#Remove punctuations and other symbols apart from english
tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)
srh<-tm_map(srh,content_transformer(removeNumbers))
srh<-tm_map(srh,removePunctuation)
srh<-tm_map(srh,content_transformer(tolower))
srh<-tm_map(srh,stripWhitespace)

library(wordcloud)
wordcloud(srh,min.freq = 5)

library(sentimentr)

#removing alpha numeric words
tweets.df$text<-gsub("[^0-9A-Za-z///+]","",tweets.df$text)
#removing http
tweets.df$text<-gsub("http\\w+","",tweets.df$text)
#removing retweets
tweets.df$text<-gsub("rt","",tweets.df$text)
#removing @
tweets.df$text<-gsub("@\\w+","",tweets.df$text)
#converting to lowercase
tweets.df$text<-tolower(tweets.df$text)

emo_srh_tweets<-sentiment(tweets.df$text)
View(emo_srh_tweets)
tweets.df$sentiment<-emo_srh_tweets$sentiment
View(tweets.df)


length(unique(tweets.df$id))

tweets_organic <- tweets[tweets$is_retweet==TRUE] 
count.fields(tweets_organic)


library(ggplot2)
library(lubridate)
ggplot(data = tweets.df, aes(x = wday(created, label =  TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Day of the week") + ylab("Number of tweets") +
  theme_minimal() +
  scale_fill_gradient(low = "turquoise3", high = "darkgreen")

ggplot(data = tweets.df, aes(x = created, fill = isRetweet)) +
  geom_histogram(bins=48) +
  xlab("Time") + ylab("Number of tweets") +
  theme_minimal() +
  scale_fill_manual(values = c("darkolivegreen3","darkolivegreen4"), name = "Retweet")
  
ggplot(data = tweets.df, aes(x = month(created, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Month") + ylab("Number of tweets") +
  theme_minimal() +
  scale_fill_gradient(low = "turquoise3", high = "darkgreen")
