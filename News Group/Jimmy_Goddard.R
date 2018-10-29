# Jimmy Goddard
# MET CS 688 Newsgroup assignment
# 10/28/18

rm(list=ls()); cat('\014')
library(tm)
library(class)

# answers
newsgroup1 <- 'sci.space'
newsgroup2 <- 'rec.autos'

Doc1.TrainPath <- system.file('texts', '20Newsgroups/20news-bydate-train/sci.space', package = 'tm')
# answer
Doc1.TrainPath <- system.path('texts', '20Newsgroups', '20news-bydate-train', newsgroup1, package = 'tm')

Doc1.TestPath <- system.file('texts', '20Newsgroups/20news-bydate-test/sci.space', package = 'tm')
Doc2.TrainPath <- system.file('texts', '20Newsgroups/20news-bydate-train/rec.autos', package = 'tm')
Doc2.TestPath <- system.file('texts', '20Newsgroups/20news-bydate-test/rec.autos', package = 'tm')

Temp1 <- DirSource(Doc1.TrainPath)
Temp2 <- DirSource(Doc2.TrainPath)
Temp3 <- DirSource(Doc1.TestPath)
Temp4 <- DirSource(Doc2.TestPath)


Doc1.Train <- Corpus(URISource(Temp1$filelist[1:100]), readerControl=list(reader=readPlain))
Doc2.Train <- Corpus(URISource(Temp2$filelist[1:100]), readerControl=list(reader=readPlain))

Doc1.Test <- Corpus(URISource(Temp3$filelist[1:100]), readerControl=list(reader=readPlain))
Doc2.Test <- Corpus(URISource(Temp4$filelist[1:100]), readerControl=list(reader=readPlain))

doc <- c(Doc1.Train, Doc1.Test, Doc2.Train, Doc2.Test)

getTransformations()

# Preprocessing: Remove punctuation, stem the documents, strip whitespace, and remove stopwords
doc.transf <- tm_map(doc, removePunctuation)
doc.transf <- tm_map(doc.transf, stemDocument)
doc.transf <- tm_map(doc.transf, stripWhitespace)
doc.transf <- tm_map(doc.transf, removeWords, stopwords('english'))

dtm <- as.matrix(DocumentTermMatrix(doc.transf, control = list(wordLengths = c(2, Inf))))
Tags <- factor(c(rep('Sci', 100), rep('Rec', 100)), levels=c('Sci', 'Rec'))

train.doc <- dtm[c(1:100, 201:300),]
test.doc <- dtm[c(101:200, 301:400),]
set.seed(0)
prob.test<- knn(train.doc, test.doc, Tags, k = 2, prob=TRUE) 
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
d <- prob.test == Tags

# classification results
(result <- data.frame(Doc=a, Predict=b, Prob=c, Correct=d))

# percentage of correct classifications
sum(prob.test == Tags) / length(Tags)

# reversed Confusion Matrix
(table(prob.test, Tags) -> AutoCM)

# manual confusion matrix
RecClassified <- (prob.test == Tags)[101:200] # Classified as 'Rec' (Positive)
SciClassified <- (prob.test == Tags)[1:100]
TP <- sum(RecClassified == 'TRUE') # Actual 'Rec' classified as 'Rec'
FN <- sum(RecClassified == 'FALSE')
FP <- sum(SciClassified == 'FALSE')
TN <- sum(SciClassified == 'TRUE')
(CM <- data.frame(Rec=c(TP,FN),Sci=c(FP,TN),row.names=c('Rec','Sci')))


# precision
(P <- 100 * TP / (TP + FP))

# recall
(R <- 100 * TP / (TP + FN))

# F-score
(f.score <- 2 * (P * R) / (P + R))

