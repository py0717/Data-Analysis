######################
# Testing Okun's Law #
######################

# Okun's Law (Differences Version) states that a 1 point increase in unemployment rate is associated with a 3 point decrease in  
#   real GDP (some texts suggest a 2 point decrease). We will test Okun's Law using annual data from Korea and Brazil.


#########
# Korea #
#########

# setup
getwd()

korea.ue <- read.csv("Korea UE Change.csv")
korea.gdp <- read.csv("Korea Real GDP Change.csv")

# data clean
korea <- merge(korea.ue, korea.gdp)
colnames(korea) <- c("DATE", "UE.Change", "GDP.Change")

head(korea)

# exploratory
attach(korea)

plot(korea)
summary(korea)

boxplot(UE.Change, main="Change in Korea's Annual Unemployment Rate")
boxplot(GDP.Change, main="% Change in Korea's Annual Real GDP")

plot(UE.Change, GDP.Change, xlab="Change in Unemployment Rate", ylab="% Change in Real GDP", main="Korea (Annual)")
abline(h=0, v=0, col="black", lty=2)

# linear regression
# delta(GDP) = alpha + beta*delta(UE) 
korea.model <- lm(GDP.Change ~ UE.Change)
summary(korea.model)      # beta-hat is significant

abline(korea.model, col="red")

detach(korea)

# diagnostic plots
plot(korea.model)  

# confidence interval test with 0.05 alpha  -> null hypothesis: beta = -3; alternative hypothesis: beta != -3
confint(korea.model, "UE.Change", level=0.95)     # fail to reject since -3 is in the CI range


##########
# Brazil #
##########

# setup
brazil.ue <- read.csv("Brazil UE Change.csv")
brazil.gdp <- read.csv("Brazil Real GDP Change.csv")

# data clean
brazil <- merge(brazil.ue, brazil.gdp)
colnames(brazil) <- c("DATE", "UE.Change", "GDP.Change")
head(brazil)

#exploratory 
attach(brazil)
plot(brazil)
summary(brazil)

boxplot(UE.Change, main="Change in Brazil's Annual Unemployment Rate")
boxplot(GDP.Change, main="% Change in Brazil's Annual Real GDP")

plot(UE.Change, GDP.Change, xlab="Change in Unemployment Rate", ylab="% Change in Real GDP", main="Brazil (Annual)")
abline(h=0, v=0, col="black", lty=2)

# linear regression 
# delta(GDP) = alpha + beta*delta(UE) 
brazil.model <-lm(GDP.Change ~ UE.Change)
summary(brazil.model)      # beta-hat is significant

abline(brazil.model, col="blue")

detach(brazil)

# diagnostic plots
plot(brazil.model)

# confidence interval test with 0.05 alpha  -> null hypothesis: beta = -3; alternative hypothesis: beta != -3
confint(brazil.model, "UE.Change", level=0.95)    # reject null since -3 is not in CI range -> beta does not equal -3
                                                  # okun's law does not hold in brazil
