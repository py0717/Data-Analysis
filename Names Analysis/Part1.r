# Assignment Part 1
# set up
library(data.table)
getwd()

# combine all of the txt files
files <- list.files(pattern = "*.TXT")
length(files)

mydata <- rbindlist(lapply(files, fread))


# data check/clean
# check that all states and DC imported (51 territories)
# check that all Gender entries are F or M
# check for any possible duplicate entries
head(mydata)
colnames(mydata) <- c("state", "gender", "year", "name", "number.of.babies")
summary(mydata)

states <- unique(mydata$state)
length(states)

unique(mydata$gender)

duplicates <- mydata[duplicated(mydata),]
head(duplicates)


# export final dataset
write.csv(mydata, "final.dataset.csv", row.names=F)
