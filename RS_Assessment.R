getwd()
setwd("C:/Users/my computer/Documents")
data <- read_excel("~/RiskSpan.xlsx", sheet = "Data")


head(data)

# Report 1
# check for possible data entry errors with unique function
unique(data$LENDER_INST_TYPE_DESCRIPTION)

banker_national <- data$CURRENT_BALANCE[which(data$LENDER_INST_TYPE_DESCRIPTION == "Bank Owned Mortgage Company - National")]
banker_bank <- data$CURRENT_BALANCE[which(data$LENDER_INST_TYPE_DESCRIPTION == "Mortgage Banker - Bank Owned")]
banker_large <- data$CURRENT_BALANCE[which(data$LENDER_INST_TYPE_DESCRIPTION == "Mortgage Banker - (Large)")]
credit_unions <- data$CURRENT_BALANCE[which(data$LENDER_INST_TYPE_DESCRIPTION == "Credit Unions")]
community_banks <- data$CURRENT_BALANCE[which(data$LENDER_INST_TYPE_DESCRIPTION == "Community Banks")]

length(banker_national)
length(community_banks)
length(credit_unions)
length(banker_large)
length(banker_bank)

summary(banker_national)
summary(community_banks)
summary(credit_unions)
summary(banker_large)
summary(banker_bank)

summary(data$CURRENT_BALANCE)

# Report 2
# check for possible data entry errors with summary 
summary(data$LTV)

ltv1 <- data$CURRENT_BALANCE[which(data$LTV <= 85)]
ltv2 <- data$CURRENT_BALANCE[which(data$LTV > 85 & data$LTV <= 90)]
ltv3 <- data$CURRENT_BALANCE[which(data$LTV > 90 & data$LTV <= 95)]
ltv4 <- data$CURRENT_BALANCE[which(data$LTV > 95)]

length(ltv1)
length(ltv2)
length(ltv3)
length(ltv4)

summary(ltv1)
summary(ltv2)
summary(ltv3)
summary(ltv4)

# Report 3
# use summary to check for data entry errors and see how many NA's
# Note: created new column in Excel file called "LOAN_AGE"
summary(data$LOAN_ORIG_DATE)

unknown <- data$CURRENT_BALANCE[is.na(data$LOAN_ORIG_DATE)]
age1 <- data$CURRENT_BALANCE[which(data$LOAN_AGE <= 9)]
age2 <- data$CURRENT_BALANCE[which(data$LOAN_AGE > 9 & data$LOAN_AGE <= 19)]
age3 <- data$CURRENT_BALANCE[which(data$LOAN_AGE > 19 & data$LOAN_AGE <= 29)]
age4 <- data$CURRENT_BALANCE[which(data$LOAN_AGE > 29 & data$LOAN_AGE <= 39)]
age5 <- data$CURRENT_BALANCE[which(data$LOAN_AGE > 39 & data$LOAN_AGE != 1361)]

length(unknown)
length(age1)
length(age2)
length(age3)
length(age4)
length(age5)

summary(unknown)
summary(age1)
summary(age2)
summary(age3)
summary(age4)
summary(age5)

# Report 4
# Sum of CURRENT_UPB by LTV and FICO
summary(data$FICO_SCORE)
boxplot(data$FICO_SCORE)

# Note: possible data entry error
# There is a recorded score of 111 but lowest possible FICO score is 300
# The report includes this possible error
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE < 600 & data$LTV <= 85)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 600 & data$FICO_SCORE <= 699 & data$LTV <= 85)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 700 & data$FICO_SCORE <= 799 & data$LTV <= 85)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 800 & data$LTV <= 85)])

sum(data$CURRENT_BALANCE[which(data$FICO_SCORE < 600 & data$LTV > 85 & data$LTV <= 90)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 600 & data$FICO_SCORE <= 699 & data$LTV > 85 & data$LTV <= 90)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 700 & data$FICO_SCORE <= 799 & data$LTV > 85 & data$LTV <= 90)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 800 & data$LTV > 85 & data$LTV <= 90)])

sum(data$CURRENT_BALANCE[which(data$FICO_SCORE < 600 & data$LTV > 90 & data$LTV <= 95)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 600 & data$FICO_SCORE <= 699 & data$LTV > 90 & data$LTV <= 95)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 700 & data$FICO_SCORE <= 799 & data$LTV > 90 & data$LTV <= 95)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 800 & data$LTV > 90 & data$LTV <= 95)])

sum(data$CURRENT_BALANCE[which(data$FICO_SCORE < 600 & data$LTV > 95)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 600 & data$FICO_SCORE <= 699 & data$LTV > 95)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 700 & data$FICO_SCORE <= 799 & data$LTV > 95)])
sum(data$CURRENT_BALANCE[which(data$FICO_SCORE >= 800 & data$LTV > 95)])

# Graph
y1 <- c(0, 3260490, 60413630, 13377034)
y2 <- c(247050, 14606019, 149979009, 29368087)
y3 <- c(0, 14598016, 135188314, 16979121)
y4 <- c(0, 629990, 5359114, 1444573)

data_table <- data.frame(y1, y2, y3, y4)
data_matrix <- as.matrix(data_table)
rownames(data_matrix) <- c("< 600", "600-699", "700-799", ">= 800")
colnames(data_matrix) <- c("<=85%", ">85% & <=90%", ">90% & <=95%", ">95%")

?barplot

barplot(data_matrix, beside = TRUE,  main = "Sum of Current Unpaid Balances by LTV Levels and FICO Scores", 
        xlab = "LTV levels", ylab = "Sum of Current UPB", col = c("black", "red", "blue","green"), 
        ylim = c(0,200000000), legend = rownames(data_matrix), names.arg = c("<=85%", ">85% & <=90%", ">90% & <=95%", ">95%"))
