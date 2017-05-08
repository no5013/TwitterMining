library(shiny)
require(twitteR)
require(RCurl)
require(tm)
require(wordcloud)
require(ggplot2)

ui <- fluidPage(
  textInput(inputId="search", label="search word"),
  textInput(inputId="n", label="N"),
  actionButton(inputId="GO", label="GO"),
  
  plotOutput("wordCloud")
)

server <- function(input, output) {
  #twitter api connect
  access_token <- "2463310314-8BgnBhpyv0pg0PyUF3CUZbaCwuWHd28wI6yLsqx"
  access_token_secret <- "OOjgr7OG5FdPgynWed2s3dyXAdk6veMQc6jw4sMddXvKB"
  consumer_key <- "M3U95oTIvJWo7tmYOhA38vm5a"
  consumer_secret <- "pl2jZ1xIbcVbO4DEO5JOAipqyMR7qnHWTk6WK02T7YWZPg3V5D"
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
  
  observeEvent(input$GO, {
    output$wordCloud <- renderPlot({
      
      #search word from twitter store in word_tweets ,change to text
	word_tweets <- searchTwitter("Guardians Galaxy 2" ,n=500, lang="en")
	word_strip <- strip_retweets(word_tweets, strip_manual = TRUE, strip_mt = TRUE)
	word_text <- sapply( word_strip, function(x) x$getText())
	word_corpus <- Corpus(VectorSource(word_text))

	#data-preprocessing
	removeUser <- function(x) gsub("@[^[:space:]]*", "", x)
	word_clean <- tm_map(word_corpus, content_transformer(removeUser))
	word_clean <- tm_map(word_clean, removePunctuation)
	word_clean <- tm_map(word_clean, content_transformer(tolower))
	word_clean <- tm_map(word_clean,removeWords, stopwords("english"))
	removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
	word_clean <- tm_map(word_clean, content_transformer(removeURL))
	removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
	word_clean <- tm_map(word_clean, content_transformer(removeNumPunct))
	word_clean <- tm_map(word_clean,stripWhitespace)
	word_clean <- tm_map(word_clean,removeWords, c("Guardians Galaxy 2"))

	#term document
	word_tdm <- TermDocumentMatrix(word_clean,control = list(wordLengths = c(1, Inf)))
	idx <- which(dimnames(word_tdm)$Terms %in% c("guardians", "galaxy"))

	#inspect frequent words
	(freq.terms <- findFreqTerms(word_tdm, lowfreq = 10))
	term.freq <- rowSums(as.matrix(word_tdm))
	term.freq <- subset(term.freq, term.freq >= 10)
	word_df <- data.frame(term = names(term.freq), freq = term.freq)

	#plot bargraph
	ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
	
	#wordcloud
	wordcloud(word_clean,max.word=30)

    })
  })
}

shinyApp(ui = ui, server = server)
