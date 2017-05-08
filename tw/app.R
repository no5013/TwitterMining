require(twitteR)
require(RCurl)
require(tm)
require(wordcloud)

#twitter api connect
access_token <- "2463310314-8BgnBhpyv0pg0PyUF3CUZbaCwuWHd28wI6yLsqx"
access_token_secret <- "OOjgr7OG5FdPgynWed2s3dyXAdk6veMQc6jw4sMddXvKB"
consumer_key <- "M3U95oTIvJWo7tmYOhA38vm5a"
consumer_secret <- "pl2jZ1xIbcVbO4DEO5JOAipqyMR7qnHWTk6WK02T7YWZPg3V5D"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)

#search word from twitter store in word_tweets ,change to text
word_tweets <- searchTwitter("Justin", n=500 , lang="en",resultType="recent")
word_text <- sapply( word_tweets, function(x) x$getText())
word_corpus <- Corpus(VectorSource(word_text))

#data-preprocessing
word_clean <- tm_map(word_corpus, removePunctuation)
word_clean <- tm_map(word_clean, content_transformer(tolower))
word_clean <- tm_map(word_clean,removeWords, stopwords("english"))
word_clean <- tm_map(word_clean,removeNumbers)
word_clean <- tm_map(word_clean,stripWhitespace)
word_clean <- tm_map(word_clean,removeWords, c("Justin"))

#wordcloud
wordcloud(word_clean)


