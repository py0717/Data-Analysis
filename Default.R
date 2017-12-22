# using Default dataset from ISLR package
library(ISLR)
data <- Default
head(data)

?Default

# exploratory analysis
plot(data)
summary(data)

boxplot(data$balance)
boxplot(data$income)

# cleaning the data
# check the qualitative variables to see if entries other than "Yes" or "No"
# check for any duplicate entries
#
# also, assume that there was a data entry error 
# students with a $0 balance actually have a balance of $100
unique(data$default)
unique(data$student)

data$balance[which(data$student == "Yes" & data$balance == 0)] <- 100

# probit to model default on student status, CC balance and income
model1 <- glm(default ~ student + balance + income, data = data, 
         family = binomial(link = "probit"))
summary(model1)

# ols to model CC balance on student status and income 
# diagnostic plots to check assumptions
model2 <- lm(balance ~ student + income, data = data)
summary(model2)

plot(model2)

# relationship between income and CC balance: parametric vs non-parametric
# dianostic plot for simple linear regression assumptions
#
# scatterplot of income vs CC balance
# graph of the two models for comparison
model3 <- lm(balance ~ income, data = data)
summary(model3)
plot(model3)

model4 <- loess(balance ~ income, data = data)
summary(model4)

plot(data$income, data$balance, xlab = "Income", ylab = "CC Balance", 
     main = "CC Balances by Income Level")
abline(model3, col = "blue")
i <- order(data$income)
lines(data$income[i], model4$fitted[i], col = "red")
