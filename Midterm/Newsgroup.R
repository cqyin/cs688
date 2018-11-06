# Jimmy Goddard
# MET CS 688 Midterm
# Newgroup.R
# 11/5/18

############################ START Q7 ######################################
rm(list=ls()); cat('\014')
library(tm)
library(class)

sci <- 'sci.electronics'
talk <- 'talk.religion.misc'

Doc1.TrainPath <- system.file('texts', '20Newsgroups', '20news-bydate-train', sci, package = 'tm')
Doc1.TestPath <- system.file('texts', '20Newsgroups/20news-bydate-test', sci, package = 'tm')
Doc2.TrainPath <- system.file('texts', '20Newsgroups/20news-bydate-train', talk, package = 'tm')
Doc2.TestPath <- system.file('texts', '20Newsgroups/20news-bydate-test', talk, package = 'tm')

Temp1 <- DirSource(Doc1.TrainPath)
Temp2 <- DirSource(Doc2.TrainPath)
Temp3 <- DirSource(Doc1.TestPath)
Temp4 <- DirSource(Doc2.TestPath)

Doc1.Train <- Corpus(
    URISource(Temp1$filelist[1:200]), 
    readerControl = list(reader = readPlain))
Doc2.Train <- Corpus(
    URISource(Temp2$filelist[1:200]), 
    readerControl = list(reader = readPlain))

Doc1.Test <- Corpus(
    URISource(Temp3$filelist[1:200]), 
    readerControl = list(reader = readPlain))
Doc2.Test <- Corpus(
    URISource(Temp4$filelist[1:200]), 
    readerControl = list(reader = readPlain))

doc <- c(Doc1.Train, Doc1.Test, Doc2.Train, Doc2.Test)

getTransformations()

doc.transf <- tm_map(doc, removePunctuation)
doc.transf <- tm_map(doc.transf, stemDocument)
doc.transf <- tm_map(doc.transf, stripWhitespace)
doc.transf <- tm_map(doc.transf, content_transformer(tolower))
doc.transf <- tm_map(doc.transf, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(
    doc.transf, 
    control = list(wordLengths = c(2, Inf), bounds = list(global = c(5, Inf))))

Tags <- factor(c(rep('Sci', 200), rep('Talk', 200)), levels = c('Sci', 'Talk'))
train.range = c(1:200, 401:600)
test.range = c(201:400, 601:800)
train.doc <- dtm[train.range,]
test.doc <- dtm[test.range,]

set.seed(0)
prob.test<- knn(train.doc, test.doc, Tags, k = 2, prob = TRUE) 
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
d <- prob.test == Tags

SciClassified <- (prob.test == Tags)[1:200] # Classified as Sci (Positive)
TalkClassified <- (prob.test == Tags)[201:400]
TP <- sum(SciClassified == 'TRUE') 
FN <- sum(SciClassified == 'FALSE')
FP <- sum(TalkClassified == 'FALSE')
TN <- sum(TalkClassified == 'TRUE')
(CM <- data.frame(Sci = c(TP, FN), Talk = c(FP, TN), row.names = c('Sci', 'Talk')))

# precision
(P <- TP / (TP + FP))

# recall
(R <- TP / (TP + FN))

# F-score
(f.score <- 2 * (P * R) / (P + R))
# [1] 0.7782609
############################ END Q7 ########################################


############################ START Q8 ######################################
subject.list <- list()
for (ff in 1:length(doc)) {
    temp <- unlist(doc[[ff]][1])
    for (ff1 in 1:length(temp)) {
        TextLine <- temp[ff1]
        if (grepl('Subject: ', TextLine)) {
            subject.list[ff] <- gsub('Subject: ', '', TextLine)
            doc[[ff]]$meta$Subject <- gsub('Subject: ', '', TextLine)
        }
    }
}

doc_ids <- names(as.matrix(dtm)[,1])
sl.df <- data.frame(doc_id = doc_ids, text = unlist(subject.list))
sl.corpus <- Corpus(DataframeSource(sl.df))

sl.corpus.transf <- tm_map(sl.corpus, removePunctuation)
sl.corpus.transf <- tm_map(sl.corpus.transf, stemDocument)
sl.corpus.transf <- tm_map(sl.corpus.transf, stripWhitespace)
sl.corpus.transf <- tm_map(sl.corpus.transf, content_transformer(tolower))
sl.corpus.transf <- tm_map(sl.corpus.transf, removeWords, stopwords('english'))

sl.dtm <- DocumentTermMatrix(
    sl.corpus.transf, 
    control = list(wordLengths = c(2, Inf), bounds = list(global = c(5, Inf))))

train.doc <- sl.dtm[train.range,]
test.doc <- sl.dtm[test.range,]

set.seed(0)
prob.test <- knn(train.doc, test.doc, Tags, k = 2, prob = TRUE) 
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
d <- prob.test == Tags

SciClassified <- (prob.test == Tags)[1:200] # Classified as Sci (Positive)
TalkClassified <- (prob.test == Tags)[201:400]
TP <- sum(SciClassified == 'TRUE') 
FN <- sum(SciClassified == 'FALSE')
FP <- sum(TalkClassified == 'FALSE')
TN <- sum(TalkClassified == 'TRUE')
(CM <- data.frame(Sci = c(TP, FN), Talk = c(FP, TN), row.names = c('Sci', 'Talk')))

# precision
(P <- TP / (TP + FP))

# recall
(R <- TP / (TP + FN))

# F-score
(f.score <- 2 * (P * R) / (P + R))
# [1] 0.8073022
############################ END Q8 ########################################
