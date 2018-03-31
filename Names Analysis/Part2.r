# Assignment Part 2
# set up
getwd()

mydata <- read.csv("final.dataset.csv")

head(mydata)
summary(mydata)

# separate males and females  
# obtain all unique names used for both genders (overlap of venn diagram)
boydata <- mydata[which(mydata$gender == "M"), c(4,5)]
girldata <- mydata[which(mydata$gender == "F"), c(4,5)]

boydata2 = unique(boydata$name)
girldata2 = unique(girldata$name)
shared.names = intersect(girldata2, boydata2) 


# sum all names for each group
# create table with boy and girl totals, discard small sample sizes, and clean
# calculate proportion for each total
boydata <- aggregate(boydata$number.of.babies, by = list(Category=boydata$name), FUN=sum)
girldata <- aggregate(girldata$number.of.babies, by = list(Category=girldata$name), FUN=sum)

gender.counter <- merge(boydata, girldata, by="Category")
colnames(gender.counter) <- c("name", "males", "females")
gender.counter <- gender.counter[which(gender.counter$males > 1000),]


gender.counter$male.prop <- gender.counter$males/(gender.counter$males + gender.counter$females)
gender.counter$female.prop <- (1-gender.counter$male.prop)

# add column that indicates how far proportion is from 0.5 (absolute distance)
gender.counter$distance <- abs(gender.counter$male.prop - 0.5)


# subset data for names with absolute distance less than or equal to 0.05
gender.counter <- subset(gender.counter, distance <= 0.05)


# order the data according to how close the male/female proportions are to being equal (smallest prop.distance)
gender.counter <- gender.counter[order(gender.counter$distance),]


# delete "Unknown" name
# add corresponding row numbers
gender.counter <- subset(gender.counter[which(gender.counter$name != "Unknown"),])
row.names(gender.counter) <- 1:length(gender.counter$name)
gender.counter <- head(gender.counter, 10)


# table of top 10 gender-neutral names
gender.counter


# I chose to search for gender-neutral names by looking for names given to an equal number of males and 
# females, a 50/50 split. This is useful because it shows if the general public perceives a name to be
# unisex. 
#
# This analysis covers the time span of 1910-2016. Therefore, it does not take into account if there was a 
# change in the genders that a name is assigned to over time. For example, it may be the case that a name 
# XYZ was given primarily to males in the past but is now primarily given to females. It would be interesting 
# to do this research in smaller increments of time (maybe 5 or 10 years) to see this.



