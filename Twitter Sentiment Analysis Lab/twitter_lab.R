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
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}

sox.tag <- '#RedSox'
yankees.tag <- '#Yankees'
tweets.sox <- searchTwitter(sox.tag)
tweets.yanks <- searchTwitter(yankees.tag)
sox.data.corpus <- getCorpus(tweets.sox)
yanks.data.corpus <- getCorpus(tweets.yanks)

# Lexicons
pos.words = scan('Data/positive-words.txt',
 what='character',
 comment.char=';')

neg.words = scan('Data/negative-words.txt',  
 what='character', 
 comment.char=';')

