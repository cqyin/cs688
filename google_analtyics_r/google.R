# /usr/bin/env R
rm(list=ls()); cat('\014')

library(googleAnalyticsR)

## This should send you to your browser to authenticate your email.
## Authenticate with an email that has access to the Google Analytics View you want to use.
ga_auth()

## get your accounts
account_list <- ga_account_list()

## pick a profile ID with data to query
# my_id <- account_list[1,'viewId']
my_id <- ga_account_list()$viewId[1]

start_date <- '2016-04-01'
end_date <- '2016-09-30'
metrics.topic <- c('pageviews', 'sessions', 'bounceRate', 'totalConversions')
dimensions.topic <- c('date', 'hour', 'minute', 'pagePath', 'source', 'medium',
    'landPagePath')

# page view query
page_views <- google_analytics(my_id, date_range = c(start_date, end_date),
    metrics = metrics.topic[1], dimensions = dimensions.topic[1])
