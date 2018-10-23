# cleaning and  preprocessing data

rm(list=ls()); cat('\014'); # clear workspace and console
library(tm)


library(stringr)
unlist(strsplit('a.b.c', '\\.'))

str_length('Web Analytics')
str_length(NA)
nchar(NA)

# training vs validation vs testing
# 60% vs 20% vs 20% usually if possible


# BE SURE TO SPECIFY THE ORDER OF THE FACTORS TO MATCH THE ORDER OF THE DOCUMENTS. 
# use the "levels" argument to the factors function to specify the order of the levels
