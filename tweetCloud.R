# EnsurePackage(x) - Install and load a package if necessary.
EnsurePackage <- function(x) {
  x <- as.character(x)
  if(!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org", dep=TRUE)
    require(x, character.only=TRUE)
  }
}

# PrepareTwitter()- Load the necessary packages for twitteR package.
PrepareTwitter <- function() {
  EnsurePackage("bitops")
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  EnsurePackage("twitteR")
}

# TweetFrame - Return a dataframe of n tweets containing searchTerm
#              sorted by creation date.
TweetFrame <- function(searchTerm, n) {
  PrepareTwitter()
  # Fetch n tweets matching the search term
  tweetList <- searchTwitter(searchTerm, n=n)
  # Convert each list element into a data frame and bind them by rows
  tweetDataFrame <- do.call("rbind", lapply(tweetList, as.data.frame))
  # Sort the data frame by date of creation
  tweetDataFrame <- tweetDataFrame[order(as.integer(tweetDataFrame$created)), ]
  return(tweetDataFrame)
}

# CleanTweets - Take the junk out of a vector of tweets.
CleanTweets <- function(tweets) {
  EnsurePackage("stringr")
  # Remove all redundants spaces
  tweets <- str_replace_all(tweets, " +", " ")
  # Get rid of URLs
  tweets <- str_replace_all(tweets, "http://t.co/[a-zA-Z0-9]{8}", "")
  # Take out RT headers (only one)
  tweets <- str_replace(tweets, "RT @[a-zA-Z_]+: ?", "")
  # Get rid of hastags
  tweets <- str_replace_all(tweets, "#[a-zA-Z]+ ?", "")
  # Get rid of references to other screenames
  tweets <- str_replace_all(tweets, "@[a-zA-Z]+ ?", "")
  # Strip out the endings characters
  tweets <- str_trim(tweets)
  return(tweets)
}

# Index - Return a term-to-document matrix from a text
#         Apply a bunch of preprocessing steps
Index <- function(text) {
  EnsurePackage("tm")
  corpus <- Corpus(VectorSource(text))
  # Prevent invalid input error
  corpus <- tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
  # Convert to lower case
  corpus <- tm_map(corpus, tolower)
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Remove English stop words
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Build and return the term-to-document matrix
  TermDocumentMatrix(corpus)
}

# WordCloud - Display a word cloud based on a matrix
WordCloud <- function(matrix) {
  EnsurePackage("wordcloud")
  cloudFrame <- data.frame(word=names(matrix), freq=matrix)
  wordcloud(cloudFrame$word, cloudFrame$freq)
}

# TweetCloud - Display a word cloud based on the most frequent terms in n tweets 
#              matching a search term
TweetCloud <- function(searchTerm, n) {
  # Fetch n tweets
  tweetDataFrame <- TweetFrame(searchTerm, n)
  # Clean them
  cleanText <- CleanTweets(tweetDataFrame$text)
  # Put them in a term-to-document matrix
  tweetTDM  <- Index(cleanText)
  # Convert it in a matrix sorted by term frequency
  matrix <- sort(rowSums(as.matrix(tweetTDM)), decreasing=TRUE)
  # Build a word cloud from it
  WordCloud(matrix)
}