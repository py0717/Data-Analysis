# Ovarian Cancer Survival Analysis

# set up
library(survival)

getwd()
setwd("C:/Users/my computer/Documents/")

cancer.data <- read.csv("ovarian cancer data.csv")
head(cancer.data)
attach(cancer.data)

# data clean
unique(status)
unique(treatment)

summary(time)
summary(age)

unique(duplicated(cancer.data))

# exploratory analysis
summary(cancer.data)

plot(cancer.data)

boxplot(time)
boxplot(status)
boxplot(treatment)
boxplot(age)

# Kaplan-Meier non-parametric analysis
kaplan1 <- survfit(Surv(time, status) ~ 1) 
summary(kaplan1)

plot(kaplan1, xlab="Time (in days)", ylab="Probability", main="Kaplan-Meier Analysis")

# Kaplan-Meier non-parametric by group (treatment type)
kaplan2 <- survfit(Surv(time, status) ~ treatment)
summary(kaplan2)

plot(kaplan2, xlab="Time (in days)", ylab="Probability", main="Kaplan-Meier Analysis Depending on Treatment",
     col=c("red", "blue"))
legend('bottomleft', c("Treatment 1", "Treatment 2"), col=c("red", "blue"), lty=1)

# Cox Proportional Hazard Models - Coefficients and Hazard Rates
# Independent variables are treatment and age
vars <- cbind(treatment, age)

cox <- coxph(Surv(time, status) ~ vars, method ="breslow")
summary(cox)
