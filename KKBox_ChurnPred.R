

# Clasiffication: KKBox Churn prediction challenge
#predict whether a user will churn after his/her subscription expires. 
#Specifically, we want to forecast if a user make a new service subscription transaction within 30 days 
#after the current membership expiration date.

library(data.table)

setwd("C:/Users/dtuiran/Documents/Machine Learning/Group Project/Data")

#Reading the data provided by Keegle

# Train: Subscription Expiration date in February, prediction for March
train<-fread("train.csv")

#Expiration for March, prediction for April
trainv2<-fread("train_v2.csv")

# Test data, predictions for April 2017 
test<-fread("sample_submission_zero.csv")

# Test data, predictions for April 2017 
test_v2<-fread("sample_submission_v2.csv")

# transactions of users up until 2/28/2017.
transactions<-fread("transactions.csv")
View(transactions[1:100,])

#newdata <- transactions[order(msno),]
#View(newdata[1:100,])

# transactions data until 3/31/2017.
transactions2<-fread("transactions_v2.csv")
View(transactions2[1:100,])

#daily user logs describing listening behaviors of a user. 
#Data collected until 2/28/2017.
user_logs<-fread("user_logs.csv")
user_logs<-user_logs[order(msno),]

#contains the user logs data until 3/31/2017.
user_logs2<-fread("user_logs_v2.csv")

View(user_logs[1:100,])

#user information, with out exp date from members 
members<-fread("members_v3.csv")
View(members[1:100,])






