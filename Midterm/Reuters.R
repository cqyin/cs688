# Jimmy Goddard
# MET CS 688 Midterm
# Reuters.R
# 11/5/18

rm(list=ls()); cat('\014')
library(tm)

# Q1
acq.dir <- system.file('texts', 'acq', package = 'tm')

# Q2
acq <- DirSource(acq.dir)
reuters <- Corpus(
    URISource(acq$filelist), 
    readerControl = list(reader = readPlain))

# Q3
reuters[[4]]$meta

# Q4
reuters[[4]]$meta$description <- 'Midterm Exam Test'
reuters[[7]]$meta$description <- 'Midterm Exam Test'
reuters[[25]]$meta$description <- 'Midterm Exam Test'

# Q5
filtered.reuters <- reuters[meta(reuters, 'description') == 'Midterm Exam Test']
filtered.dtm <- DocumentTermMatrix(filtered.reuters)
findFreqTerms(filtered.dtm, 10)

# Q6
freq <- colSums(as.matrix(filtered.dtm))
WordFreq <- data.frame(word = names(freq), freq = freq)
as.vector(subset(WordFreq, freq > 10)$word)
