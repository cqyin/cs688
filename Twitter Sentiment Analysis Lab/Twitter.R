rm(list=ls()); cat('\014')
t.api.key <- 'IlhDaNfS5nhfR3EhTApTgHxDZ'
t.api.secret <- 'aHHwJGKZqhjyJQCsVbFXvFk9d2u9hKQILmgdXgBFmFxYIjvF6b'
t.api.access_token <- '122929309-Bjhw82SBFgGaGuB2c3XxicK1XT7SuG0RYFffyj4J'
t.api.access_secret <- '5XZlOklveiH1YevZ0yxzhGKolUFnm3j6Xo9r2KJAeeDZ0'

library('twitteR')
library('ROAuth')

setup_twitter_oauth(t.api.key, t.api.secret, access_token = t.api.access_token, access_secret = t.api.access_secret)
start <- getUser('cnnbrk')
start$description

start$name
start$screenName
start$id
tweets1 <- searchTwitter('#bigdata', n = 5)
display.tweet <- function (tweet) {
  cat('Screen name:', tweet$getScreenName(), '\nText:', tweet$getText(), '\n\n')
}

for (t in tweets1) {
  display.tweet(t)
}


# trends
display.trend <- function (trend) {
    cat('Name:', trend$name, '\n url:', trend$url, '\n\n')
}

# available trends
trends.locations <- availableTrendLocations()
head(trends.locations)

trends.location.woeid <- trends.locations[1, 'woeid']
trends1 <- getTrends(trends.location.woeid)
for (i in 1:nrow(trends1)) {
    display.trend(trends1[i,])
}

# example: friends/followers tweets
user <- getUser('kaggle')
user.name <- user$name
friends <- lookupUsers(user$getFriendIDs(n = 15))
followers <- lookupUsers(user$getFollowerIDs(n = 5))
friends.names <- sapply(friends, name)
followers.names <- sapply(followers, name)
friends.frame <- data.frame(User = user.name, Follower = friends.names)
followers.frame < data.frame(User = followers.names, Follower = user.name)
relations <- merge(friends.frame, followers.names, all = TRUE)

library(igraph)

g <- graph.data.frame(relations, directed = TRUE)
V(g)[2:6]$color <- 'blue'
V(g)[7:21]$color <- 'red'
plot(g)

library(tm)
library(SnowballC)

tweets <- userTimeline("kaggle", n = 200)
tweets.text <- lapply(tweets, function(t) { t$getText() })
data.source <- VectorSource(tweets.text)
data.corpus <- Corpus(data.source)

# tdm <- TermDocumentMatrix(data.corpus)

# inspect the first two values
inspect(data.corpus[1:2])

meta(data.corpus[[1]])
content(data.corpus[[1]])

# transformations

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(tolower))

inspect(data.corpus[1:2])

removeURL <- function(x) {
  gsub("(http[^ ]*)", "", x)
}

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(removeURL))

inspect(data.corpus[1:2])

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(removePunctuation))

english.stopwords <- stopwords("en")
head(english.stopwords)

data.corpus <- 
  tm_map(data.corpus,
         content_transformer(removeWords),
         english.stopwords)

# inspect the first two values
inspect(data.corpus[1:2])

removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", "", x)
}

data.corpus <- 
  tm_map(data.corpus, 
         content_transformer(removeNumberWords))


# inspect the first two values
inspect(data.corpus[1:2])


data.corpus <- 
  tm_map(data.corpus,
         content_transformer(stemDocument))

data.corpus <- 
  tm_map(data.corpus,
         content_transformer(stripWhitespace))

# inspect the first two values
inspect(data.corpus[1:2])
