# import json file and create small multiples graphs
library(jsonlite)
library(tidyr)
library(ggplot2)
library(dplyr)

# set up
getwd()
yelp <- stream_in(file('yelp_academic_dataset_business.json'))

# clean
# subset data to keep only relevant columns
# obtain top10 most frequent categories and subset data again
dim(yelp)
head(yelp)
colnames(yelp)

yelp <- yelp[c('stars', 'categories')]
yelp <- yelp %>% unnest(categories)

top10 <- sort(table(yelp$categories), decreasing=TRUE)[1:10]
top10 <- names(top10)
yelp <- subset(yelp, categories %in% top10)
unique(yelp$categories)

# exploratory
summary(yelp)
boxplot(yelp$stars, main="Distribution of Stars (Top10)")

# small multiples graph
ggplot(yelp, aes(x=stars)) + geom_histogram(binwidth=0.5) + facet_wrap(~ categories, nrow=2)
