# Example: Shiny app that search Wikipedia web pages
# File: server.R
library(shiny)
library(tm)
library(stringi)
# library(proxy)
library(wordcloud)
source("WikiSearch.R")


shinyServer(function (input, output) {
  output$distPlot <- renderPlot({
    result <- SearchWiki(input$select)
    set.seed(123)
    wordcloud(names(result), result, max.words = 50, colors = brewer.pal(6, "Dark2"))
  })
})

