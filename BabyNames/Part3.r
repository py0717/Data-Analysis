# Assignment Part 3

# set up
library(dplyr)   

getwd()
mydata <- read.csv("final.dataset.csv")


# Part A
# find states that are "trend setters" (2012-2016)
# define "trend setters" as states that pick names not frequently used
# define "frequently used" as more than 5000 instances from previous 5 years (2007-2011)


# subset 2007-2011 data
# sum each name in this period and get frequent names
frequent <- mydata[(mydata$year >= 2007 & mydata$year <= 2011),]
frequent <- aggregate(frequent$number.of.babies, by=list(Category=frequent$name), FUN=sum)
frequent <- frequent[which(frequent$x > 5000),]


# subset 2012-2016 data
mydata2 <- mydata[which(mydata$year >= 2012 & mydata$year <= 2016),]

# table of total number of babies in each state (2012-2016)
total.count <- aggregate(mydata2$number.of.babies, by=list(Category=mydata2$state), FUN=sum) 

# subset of 2012-2016 data to exclude frequent names
mydata2 <- mydata2[!(mydata2$name %in% frequent$Category),]

# table of total number of babies not given frequently used names 
not.frequent.count <- aggregate(mydata2$number.of.babies, by=list(Category=mydata2$state), FUN=sum) 

# merge tables, get proportions, clean
combined1 <- merge(not.frequent.count, total.count, by="Category")
colnames(combined1) <- c("state", "not.frequent", "total.babies")
combined1$proportion <- combined1$not.frequent/combined1$total.babies
combined1 <- combined1[order(-combined1$proportion),]
combined1 <- head(combined1, 10)
row.names(combined1) <- 1:10

# table of top 10 "trend setting" states and proportion of babies given infrequent names
combined1   



# Part B
# finding states that adopt popular names
# define "popular name" as name that was one of the top 50 from previous 5 years
# top 50 is in terms of total count


# subset 2007-2011 data
# sum each name in this period and get top 50 counts
# clean table
top50 <- mydata[which(mydata$year >= 2007 & mydata$year <= 2011),]
top50 <- aggregate(top50$number.of.babies, by=list(Category=top50$name), FUN=sum)

top50 <- top50[order(-top50$x),]
top50 <- head(top50, 50)
row.names(top50) <- 1:length(top50$Category)
colnames(top50) <- c("name", "count")
top50     # table of popular names ordered by counts


# subset 2012-2016 data
mydata2 <- mydata[which(mydata$year >= 2012 & mydata$year <= 2016),]  

# table of total number of babies in each state (2012-2016)
total.count <- aggregate(mydata2$number.of.babies, by=list(Category=mydata2$state), FUN=sum) 

# subset mydata2 to only contain top 50 names
mydata2 <- mydata2[mydata2$name %in% top50$name,] 

# table of total number of babies in each state (2012-2016) with popular names
popular.count <- aggregate(mydata2$number.of.babies, by=list(Category=mydata2$state), FUN=sum) 


# merge tables, get proportions, clean
combined2 <- merge(popular.count, total.count, by="Category")
colnames(combined2) <- c('state','pop.names','total.babies')
combined2$proportion <- combined2$pop.names/combined2$total.babies
combined2 <- combined2[order(-combined2$proportion),]
row.names(combined2) <- 1:51
combined2 <- head(combined2, 10)

# table of top 10 states that adopt popular names
combined2     



# Part A
# I defined “frequently used” as 5000 because of the timeframe that I assessed (five 
# years).  I believe five years is a fair time span for this study as most people do not 
# have babies frequently enough for us to look at this on a year-to-year basis. 
#
# Creating a subset of the 2012-2016 data by eliminating frequent names gives us a 
# dataset that keeps infrequently used names as well as any new names not used in 
# the previous period. This method allows us to determine the proportion of people that 
# give their children uncommon or new names.
#
# Creating a subset of the 2012-2016 data by matching infrequent names from the previous 
# period would be incorrect here. It would give us a dataset that excludes any names that 
# are brand new and not used at all in the previous period.
#
# My results indicate that Texas, New York, California, Florida, Georgia, Louisiana, Utah, 
# Illinois, Ohio and Washington have the highest proportion of babies that were not given 
# frequently used names. 
#
# This study is limited in that it does not take into account other previous time periods. 
# Also, it would be possible for us to overlook a trend that started late 2011 with a 
# completely new name that spilled over to the new time period. All of the people that picked 
# the same name due to its recent popularity would be count as a trend setter and not follower. 
    



# Part B
# I defined popular as top 50 arbitrarily (we could easily reassess the data with 100, 200, 
# etc). I picked 5 years again as the time frame for similar reasons as in Part A. 
#
# For this problem, I chose to subset the 2012-2016 data with the top 50 names from the 
# previous time period. This makes sense since people that follow a trend may be inclined to 
# see a list of popular names and simply pick one from there.
# 
# My results indicate that people in Rhode Island, Delaware, Vermont, New Hampshire, 
# Connecticut, Wyoming, DC, Alaska,  Massachusetts and Hawaii are most likely to give their 
# child a name that is already popular.
#
# The limitations here are similar to in Part A.
