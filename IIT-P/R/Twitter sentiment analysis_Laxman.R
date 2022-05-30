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
tweets<-searchTwitter("CSKvSRH",n=500,since = "2020-07-01",lang = "en")
#count the number of tweets
n.tweets<-length(tweets)
#convert tweets to data frame
tweets.df<-twListToDF(tweets)
View(tweets.df)

# Getting the hashtags from the list format
tweets_hash <- unlist
                     ‘^c\\(|,|”|\\)’))
# Formatting by removing the white spacea
bg_tags <- sapply(bg_tags_split,
          function(y) nchar(trimws(y)) > 0 & !is.na(y))

bg_tag_df <- as_data_frame(table(tolower(bg_tags_split[bg_tags])))

bg_tag_df <- bg_tag_df[with(bg_tag_df,order(-n)),]

bg_tag_df <- bg_tag_df[1:10,]

ggplot(bg_tag_df, aes(x = reorder(Var1, -n), y=n)) +
 geom_bar(stat=“identity”, fill=“steelblue”)+
 theme_minimal() +
 xlab(“#Hashtags”) + ylab(“Count”)

