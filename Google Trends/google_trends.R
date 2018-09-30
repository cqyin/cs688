# Google Trends
rm(list=ls()); cat('\014') # clear all
library(readr)

GT.Data <- read.csv(
  paste0('Data/', 'geoMap.csv'), 
  stringsAsFactors = FALSE,
  skip = 2, 
  blank.lines.skip = TRUE, 
  header = T)

colnames(GT.Data) <- c('Region', 'GB', 'GG')
GT.Data[1:5,]

# convert to numeric values
zGB <- as.numeric(GT.Data$GB) # gift for boyfriend
zGG <- as.numeric(GT.Data$GG) # gift for girlfriend

# place back to dataframe
GT.Data$GB <- zGB
GT.Data$GG <- zGG

# find NA and replace with zero
ix1 <- which(is.na(GT.Data$GB))
GT.Data$GB[ix1] <- 0
ix2 <- which(is.na(GT.Data$GG))
GT.Data[ix2,]
GT.Data$GG[ix2] <- 0

# Q1: Which are the states where GG is smaller than 1? Find those and replace
# then with zero

# find states where GG is less than one
gg.less.than.one <- which(GT.Data$GG < 1)
GT.Data$Region[gg.less.than.one]
# [1] "South Dakota" "Maine"        "Idaho"

# replace with zero the GG value for rows where GG is less than one
GT.Data$GG[gg.less.than.one] <- 0

# Q2: How many states GB > GG
nrow(GT.Data[with(GT.Data, GB > GG),])
nrow(GT.Data[GT.Data$GB > GT.Data$GG,])
# [1] 46

# Q3: Find any states where GG + 10 > GB
GT.Data$Region[GT.Data$GG + 10 > GT.Data$GB]
# [1] "Washington"

# G4: What is the % of states for which GG + 10 > GB
(nrow(GT.Data[GT.Data$GG + 10 > GT.Data$GB,]) / nrow(GT.Data)) * 100
# [1] 2.173913

# Q5: What is the ratio GG/GB for the state of New Hamshire
new.hampshire <- GT.Data[GT.Data$Region == 'New Hampshire',]
new.hampshire$GG / new.hamsphire$GB
# [1] 0.5

# Q6: Create a bar plot fo GG & GB values for each state
barplot(
  rbind(GT.Data$GG, GT.Data$GB), 
  beside = TRUE, 
  names.arg = GT.Data$Region, 
  las = 2, 
  col = c('pink', 'lightBlue'))

