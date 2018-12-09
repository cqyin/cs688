# put the lexical files and whatever else we need in a Data folder
# create a project, finish the work and zip it, upload it to blackboard
# word cloud 3 or more freq

rm(list=ls()); cat('\014')
t.api.key <- 'IlhDaNfS5nhfR3EhTApTgHxDZ'
t.api.secret <- 'aHHwJGKZqhjyJQCsVbFXvFk9d2u9hKQILmgdXgBFmFxYIjvF6b'
t.api.access_token <- '122929309-Bjhw82SBFgGaGuB2c3XxicK1XT7SuG0RYFffyj4J'
t.api.access_secret <- '5XZlOklveiH1YevZ0yxzhGKolUFnm3j6Xo9r2KJAeeDZ0'

library(twitteR)
library(ROAuth)
library(tm)
library(SnowballC)
library(wordcloud)

setup_twitter_oauth(t.api.key, t.api.secret, access_token = t.api.access_token, access_secret = t.api.access_secret)

getText <- function (w) {
  w$getText()
}

getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, getText)
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return(data.corpus)
}

removeURL <- function (x) {
  gsub("(http[^ ]*)", "", x)
}

removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}


preprocess <- function (data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  english.stopwords <- stopwords("en")
  data.corpus <- tm_map(data.corpus, content_transformer(removeWords), english.stopwords)
  data.corpus <- tm_map(data.corpus, content_transformer(removeNumberWords))
  data.corpus <- tm_map(data.corpus, content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus, content_transformer(stripWhitespace))
  return(data.corpus)
}

sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0) {
    return(NA)
  }
  return(p - n)
}

sox.tag <- '#RedSox'
yankees.tag <- '#Yankees'
tweets.sox <- searchTwitter(sox.tag)
tweets.yanks <- searchTwitter(yankees.tag)
data.corpus1 <- getCorpus(tweets.sox)
data.corpus2 <- getCorpus(tweets.yanks)

data.trans.corpus1 <- preprocess(data.corpus1)
data.trans.corpus2 <- preprocess(data.corpus2)

tdm1 <- TermDocumentMatrix(data.trans.corpus1)
tdm2 <- TermDocumentMatrix(data.trans.corpus2)

FFT1 <- findFreqTerms(tdm1, lowfreq = 3)
FFT2 <- findFreqTerms(tdm2, lowfreq = 3)
wordFreq1 <- rowSums(as.matrix(tdm1))
wordFreq2 <- rowSums(as.matrix(tdm2))

palette <- brewer.pal(8, 'Dark2')
set.seed(137)

FFT1
wordcloud(
  words = names(wordFreq1),
  freq = wordFreq1,
  min.freq = 3,
  random.order = FALSE,
  colors = palette)

FFT2
wordcloud(
  words = names(wordFreq2),
  freq = wordFreq2,
  min.freq = 3,
  random.order = FALSE,
  colors = palette)

# Lexicons
pos.words = scan('Data/positive-words.txt',
 what='character',
 comment.char=';')

neg.words = scan('Data/negative-words.txt',
 what='character',
 comment.char=';')

(sox.sentiment <- sentiment(lapply(tweets.sox, getText), pos.words, neg.words))
(yanks.sentiment <- sentiment(lapply(tweets.yanks, getText), pos.words, neg.words))
barplot(
  c(sox.sentiment, yanks.sentiment), 
  names.arg = c('Sox', 'Yanks'), 
  col = c('red', 'blue'))

# There appear to be far more negative words used in Yankees tweets than Red Sox tweets
