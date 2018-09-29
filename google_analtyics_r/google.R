# /usr/bin/env R
rm(list=ls()); cat('\014')

library(googleAnalyticsR)

## This should send you to your browser to authenticate your email.
## Authenticate with an email that has access to the Google Analytics View you want to use.
ga_auth()

## get your accounts
account_list <- ga_account_list()

## pick a profile ID with data to query
ga_id <- account_list[1,'viewId']
ga_id <- ga_account_list()$viewId[1]
