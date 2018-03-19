# Survival Analysis of Basketball Players Drafted by NBA from 2001-2018
# Time = number of games played in NBA
# Status = whether a player is inactive in the league (retired, cut, etc)
library(survival)

getwd()

data <- read.csv("bball.csv")
head(data)

data2 <- data[, c("Pk", "Tm", "Player", "Age", "Pos", "From", "To", "G")]
head(data2)


# data cleaning
# create 'Status' column; players still playing in 2018 (To column) are active (status=0)
# players with year=NA or year < 2018 (To column) are inactive in NBA (status=1)
# players with NA amount of games played 0 games
# note: 64 players without position listed; all are players with 0 games; changed to NA
summary(data2)

data2$Status <- ifelse(data2$To == 2018, 0, 1)
data2$Status[is.na(data2$Status)] <- 1
data2$G[is.na(data2$G)] <- 0
data2$Pos[which(data2$Pos=='')] <- NA


# exploratory analysis
summary(data2)

boxplot(data2$G, main="Total Games Played")
boxplot(data2$Age, main="Age of Players Drafted Into NBA")

plot(data2$Pos, main="Positions of Players Drafted")



# Kaplan-Meier analysis
kapmodel1 <- survfit(Surv(G, Status) ~ 1, data=data2)
summary(kapmodel1)

plot(kapmodel1, xlab="Number of Games", ylab="Probability", main="Kaplan-Meier Analysis")
  

# Kaplan-Meier non-parametric analysis by player's position
# note1: some players play multiple positions, most played position was used 
# note2: missing position data should not affect results too much assuming they are 
#        random with respect to position
kapmodel2 <- survfit(Surv(G, Status) ~ Pos, na.action=na.exclude, data=data2)
summary(kapmodel2)

plot(kapmodel2, xlab="Number of Games", ylab="Probability", main="Kaplan-Meier by Player's Position", 
     col=c("black", "red", "green"))
legend("topright", c("Centers", "Forwards", "Guards"), col=c("black", "red", "green"), lty=1)


# log-rank test for difference between survival times
survdiff(Surv(G, Status) ~ Pos, na.action=na.exclude, data=data2)


# Peto & Peto's generalized Wilcoxon test for difference between survival times
survdiff(Surv(G, Status) ~ Pos, na.action=na.exclude, data=data2, rho=1)


# Cox Proportional Hazard Models - Coefficients and Hazard Rates
# independent variables used: player's overall pick number, age and position
attach(data2)

vars <- cbind(Pk,Age,Pos)

cox <- coxph(Surv(G, Status) ~ vars, na.action=na.exclude, method ="breslow")
summary(cox)

detach(data2)




