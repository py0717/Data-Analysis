# using Default dataset from ISLR package
library(ISLR)
data <- Default

?Default

# data clean
# assume there was a data entry error 
# students with a $0 balance actually have a balance of $100
head(data)

unique(data$default)
unique(data$student)

duplicates <- data[duplicated(data),]
head(duplicates)

data$balance[which(data$student=="Yes" & data$balance==0)] <- 100

# exploratory analysis
plot(data)
summary(data)

boxplot(data$balance, main="Credit Card Balances")
boxplot(data$income, main="Income")

# probit to model default on student status, CC balance and income
model1 <- glm(default ~ student + balance + income, data=data, 
              family=binomial(link="probit"))
summary(model1)

# ols to model CC balance on student status and income 
# diagnostic plots to check assumptions
model2 <- lm(balance ~ student + income, data=data)
summary(model2)

plot(model2)


############################################################################
# relationship between income and CC balance: parametric vs non-parametric #
############################################################################

# OLS model
model3 <- lm(balance ~ income, data=data)
summary(model3)
plot(model3)

# LOESS model
model4 <- loess(balance ~ income, data=data)
summary(model4)

# graph: OLS vs LOESS
plot(data$income, data$balance, xlab="Income", ylab="CC Balance", 
     main="CC Balances by Income Level")
abline(model3, col="green")

i <- order(data$income)
lines(data$income[i], model4$fitted[i], col="red")

legend('topright', c('OLS', 'LOESS'), col=c('Green', "Red"), lty=1)
