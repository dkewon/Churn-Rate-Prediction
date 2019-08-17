
setwd("C:/Users/dtuiran/Documents/Machine Learning/Group Project/Data")

library(data.table)


# Train: Subscription Expiration date in February, prediction for March
train<-fread("train.csv")

#Expiration for March, prediction for April
trainv2<-fread("train_v2.csv")

# Test data, predictions for April 2017 
test<-fread("sample_submission_zero.csv")

# Test data, 
test_v2<-fread("sample_submission_v2.csv")


all(train$msno == test$msno)


table(train$is_churn)
table(test$is_churn)


info<-read.csv("train_basetable.csv")
info$X <-NULL

#Adding the other parameters
train<-merge(x = train,y=info, all.x = TRUE, by="msno")
train<-train[complete.cases(train), ]

#Adding the other parameters
test<-merge(x = test,y=info, all.x = TRUE, by="msno")
test<-test[complete.cases(test), ]
