#For Text Mining assignment

#ONE:
#1) Extract tweets for any user (try choosing a user who has more tweets)
#2) Perform sentimental analysis on the tweets extracted from the above

devtools::install_github("lchiffon/wordcloud2")
devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
devtools::install_version("httr",version="0.6.0",repos="http://cran.us.r-project.org")
install.packages("twitteR", repos = "http://cran.us.r-project.org")
install.packages("RCurl", repos = "http://cran.us.r-project.org")
install.packages("httr", repos = "http://cran.us.r-project.org")
install.packages("syuzhet", repos = "http://cran.us.r-project.org")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("httpuv")

library(httpuv)
library("ROAuth")
library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(base64enc)

cred <- OAuthFactory$new(consumerKey='ZIxPIvj21hhddkMxdjgt3wf94', # Consumer Key (API Key)
                         consumerSecret='BAxTBGvVKZNPiCRADNTNK3VrLO3wrK6OUms3bJXLSpMZ8z5IDI', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

setup_twitter_oauth("ZIxPIvj21hhddkMxdjgt3wf94", # Consumer Key (API Key)
                    "BAxTBGvVKZNPiCRADNTNK3VrLO3wrK6OUms3bJXLSpMZ8z5IDI", #Consumer Secret (API Secret)
                    "396521281-5OZ7cS7Ib90Akgdcx4ysPvZ2hQ8uFzGzXuxfxKHB",  # Access Token
                    "MDbqjOaaoMou3vfjSgEiev6omUkkatz692mQXzH8mVP3V")  #Access Token Secret

#registerTwitterOAuth(cred)
tweets = searchTwitter("#RoyalWedding", n = 10000, lang = "en")
tweets.df = twListToDF(tweets)
dim(tweets.df)
View(tweets.df)

write.csv(tweets.df, "Tweets.csv",row.names = F)

getwd()

# CLEANING TWEETS

tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")


# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #RoyalWedding")
api_create(p,filename="Sentimentanalysis")

# Create comparison word cloud data

wordcloud_tweet = c(
  paste(tweets.df$text[emotions$anger > 0], collapse=" "),
  paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(tweets.df$text[emotions$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)