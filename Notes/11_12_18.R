rm(list=ls()); cat("\014") # clear all
library('RSNNS')
data('snnsData')
laser <- snnsData$laser_1000.pat
inputs <- laser[,inputColumns(laser)]
targets <- laser[,outputColumns(laser)]
patterns <- splitForTrainingAndTest(inputs, targets, ratio = 0.15)
model <- elman(patterns$inputsTrain, patterns$targetsTrain, size = c(8, 8), 
               learnFuncParams = c(0.1), maxit= 500, inputsTest = patterns$inputsTest, 
               targetsTest = patterns$targetsTest, linOut = FALSE)
plot(c(0, 100), c(0, 0.9), type = 'n')
lines(inputs[1:100], col = 'black', lwd = 2.5)
lines(targets[1:100], col = 'blue', lwd = 2.5)
lines(model$fittedTestValues, col = 'red', lwd = 2.5)


library('edgarWebR')
ticker <- 'FRO' # company ticker
res <- company_information(ticker)
res2 <- edgarWebR::company_information(ticker)
# knitr is in base R
# no need to use 'library' to bring in the library.  you can use :: to access it
# through it's name
knitr::kable(res[,1:8], digits = 2, format.args = list(big.mark = ','))
knitr::kable(res[,9:16], digits = 2, format.args = list(big.mark = ','))
filings <- company_filings(ticker, count = 100)
