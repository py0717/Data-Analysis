# set up
getwd()
users <- read.csv("out.csv")

# exploratory
head(users)
summary(users)

##########
# Part A #
########## 

# scatterplot
# regression model
# graph model on scatterplot
attach(users)

plot(friends, average_stars, xlab='Number of Friends', ylab='Average Rating', 
     main='Scatterplot of Number of Friends and Average Rating for Yelp Users')

model <- lm(average_stars ~ friends)
summary(model)

abline(model, col='red')

detach(users)

##########
# Part B #
##########

# create copy of users data
users2 <- data.frame(users) 

# 99th percentile
quantile(users2$friends, c(0.99))    # result is 107 friends

# put a ceiling of 100 on number of friends
users2$friends[which(users2$friends > 100)] <- 100
summary(users2)

# scatterplot
# regression model
# graph model on scatterplot
attach(users2)

plot(friends, average_stars, xlab='Number of Friends', ylab='Average Rating', 
     main='Scatterplot of Number of Friends (Ceiling of 100) and Average Rating
     for Yelp Users')

model2 <- lm(average_stars ~ friends)
summary(model2)

abline(model2, col='blue')

detach(users2)
