# Survival Analysis of Basketball Players Drafted by NBA
# Time = number of games played in NBA
# Status = whether a player is inactive in the league (retired, cut, etc)
library(survival)

getwd()

data <- read.csv("bball.csv")
head(data)

data2 <- data[, c("Pk", "Tm", "Player", "Age", "Pos", "From", "To", "G")]
head(data2)
attach(data2)

# data cleaning
# create 'Status' column; players still playing in 2018 are active (status=0)
# players with NA in ending Year (To column) inactive in NBA (status=1)
# players with NA amount of games played 0 games
summary(data2)

data2$Status <- ifelse(data$To == 2018, 0, 1)
data2$Status[is.na(data2$Status)] <- 1
data2$G[is.na(data$G)] <- 0

tail(data2)

# exploratory analysis
# note: 64 players without position listed; all are players with 0 games
boxplot(data2$G)
boxplot(data2$Age)
plot(data2$Pos)

# Kaplan-Meier analysis
kapmodel1 <- survfit(Surv(G, Status) ~ 1, data=data2)
summary(kapmodel1)

plot(kapmodel1, xlab="Number of Games", ylab="Probability", main="Kaplan-Meier Survival: Players Drafted into NBA")
  

# Kaplan-Meier non-parametric analysis by player's position
# note1: some players play multiple positions, most played position was used 
# note2: missing position data should not affect results too much assuming they are 
#        random with respect to position
kapmodel2 <- survfit(Surv(G, Status) ~ Pos, data=data2)
summary(kapmodel2)

plot(kapmodel2, xlab="Number of Games", ylab="Probability", main="Kaplan-Meier Survival by Player's Position", 
     col=c("black", "red", "green"))
legend("topright", c("Guards", "Forwards", "Centers"), col=c("black", "red", "green"), lty=1)


# Cox Proportional Hazard Models - Coefficients and Hazard Rates
# independent variables used: player's overall pick number, age and position
vars <- cbind(Pk,Age,Pos)

cox <- coxph(Surv(data2$G, data2$Status) ~ vars, na.action=na.exclude, method ="breslow")
summary(cox)


