library(shiny)
require(twitteR)
require(RCurl)
require(tm)
require(wordcloud)
require(ggplot2)
source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")

ui <- fluidPage(
  textInput(inputId="search", label="search word"),
  textInput(inputId="n", label="N"),
  actionButton(inputId="GO", label="GO"),
  actionButton(inputId="RenderMap", label="RenderMap"),
  sliderInput(inputId = "graphSlider", min = 1, max = 30, value = 10, label = "Min Frequent"),
  sliderInput(inputId = "clusterSlider", min = 2, max = 10, value = 5, label = "Number of cluster"),
  
  plotOutput("wordCloud"),
  plotOutput("barGraph"),
  plotOutput("cluster"),
  verbatimTextOutput("clusterInfo")
)

server <- function(input, output) {
  #twitter api connect
  access_token <- "2463310314-8BgnBhpyv0pg0PyUF3CUZbaCwuWHd28wI6yLsqx"
  access_token_secret <- "OOjgr7OG5FdPgynWed2s3dyXAdk6veMQc6jw4sMddXvKB"
  consumer_key <- "M3U95oTIvJWo7tmYOhA38vm5a"
  consumer_secret <- "pl2jZ1xIbcVbO4DEO5JOAipqyMR7qnHWTk6WK02T7YWZPg3V5D"
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
  
  observeEvent(input$GO, {
	#search word from twitter store in word_tweets ,change to text
	word_tweets <- searchTwitter(isolate(input$search) ,n=isolate(input$n), lang="en")
	word_strip <- strip_retweets(word_tweets, strip_manual = TRUE, strip_mt = TRUE)
	word_text <- sapply( word_strip, function(x) x$getText())
	word_corpus <- Corpus(VectorSource(word_text))

	#data-preprocessing
	removeUser <- function(x) gsub("@[^[:space:]]*", "", x)
	word_clean <- tm_map(word_corpus, content_transformer(removeUser))
	word_clean <- tm_map(word_clean, removePunctuation)
	word_clean <- tm_map(word_clean,removeWords, stopwords("english"))
	removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
	word_clean <- tm_map(word_clean, content_transformer(removeURL))
	removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
	word_clean <- tm_map(word_clean, content_transformer(removeNumPunct))
	word_clean <- tm_map(word_clean,stripWhitespace)
	word_clean <- tm_map(word_clean, content_transformer(tolower))
	word_clean <- tm_map(word_clean,removeWords, c(isolate(input$search),"the","via","from","use","amp","for","just","I","i","a","link"))
	
	word_tdm <- TermDocumentMatrix(word_clean,   control = list(wordLengths = c(1, Inf)))
	
	
	
	#term document
	idx <- which(dimnames(word_tdm)$Terms %in% c(isolate(input$search)))

	#Clustering
	tdm2 <- removeSparseTerms(word_tdm, sparse = 0.95)
	m2 <- as.matrix(tdm2)
	distMatrix <- dist(scale(m2))
	
  output$wordCloud <- renderPlot({
		#wordcloud
		wordcloud(word_clean,max.word=50)
	
	})

	output$barGraph <- renderPlot({
		#inspect frequent words
		(freq.terms <- findFreqTerms(word_tdm, lowfreq = 10))
		term.freq <- rowSums(as.matrix(word_tdm))
		term.freq <- subset(term.freq, term.freq >= input$graphSlider)
		word_df <- data.frame(term = names(term.freq), freq = term.freq)

		#plot bargraph
		ggplot(word_df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
	
	})
	
	output$cluster <- renderPlot({
	
	  fit <- hclust(distMatrix, method = "ward.D2")
	  plot(fit)
	  rect.hclust(fit, k = input$clusterSlider)
	  
	})
	
	output$clusterInfo <- renderPrint({
	  m3 <- t(m2)
	  set.seed(122)
	  k <- input$clusterSlider
	  kmeansResult <- kmeans(m3, k)
	  round(kmeansResult$centers, digits = 3)
	  for (i in 1:k)
	  {
	    cat(paste("cluster ", i, ":  ", sep = ""))
	    s <- sort(kmeansResult$centers[i, ], decreasing = T)
	    cat(names(s)[1:5],"+\n")
	  }
	})
	

  })
  observeEvent(input$RenderMap, {
    twitterMap(input$search,plotType="followers", userLocation="Japan")
  })
}

shinyApp(ui = ui, server = server)
