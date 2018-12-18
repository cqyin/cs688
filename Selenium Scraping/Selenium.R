rm(list=ls()); cat("\014") # clear all

library(RSelenium)
library(XML)

# I got much of my installation information 
# from here: https://tecadmin.net/setup-selenium-chromedriver-on-ubuntu/
# running selenium locally with chromedriver:
# xvfb-run java -Dwebdriver.chrome.driver=/usr/bin/chromedriver -jar selenium-server-standalone-3.13.0.jar

# http://rpubs.com/johndharrison/RSelenium-Basics
# even more convenient (after installing docker):
# sudo docker run -d -p 4444:4444 selenium/standalone-chrome
#
# note: I modified the docker command listed in the above url to make use of 
# standalone-chrome. I didn't specify a colon or version and it grabbed "lastest"

remDr <- remoteDriver(browserName = 'chrome', port = 4444)
remDr$open()
remDr$navigate('https://www.fandango.com')
remDr$screenshot(display = T)

# find search input and enter my zip code
input <- remDr$findElement('css', 'input[name="q"]')
input$sendKeysToElement(list('02135', '\uE007'))
remDr$screenshot(display = T)
remDr$getCurrentUrl()

# find first movie link and click it to get to its page where there are reviews
first.movie.link <- remDr$findElement('css', 'h3.fd-movie__title > a')
first.move.name <- unlist(first.movie.link$getElementText())
movie.link.elems <- remDr$findElements('css', 'h3.fd-movie__title > a')

# print each url that the a element points to
for (link in movie.link.elems) {
  # movie name
  print(unlist(link$getElementText()))
  # link target
  print(unlist(link$getElementAttribute('href')))
}

first.movie.link$clickElement()
remDr$getCurrentUrl()

# extract each of the review snippets
rt.reviews <- remDr$findElements('class', 'rt-reviews-list__quote')
reviews <- c()
for (review in rt.reviews) {
  reviews <- c(reviews, unlist(review$getElementText()))
}
