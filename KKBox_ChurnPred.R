

# Clasiffication: KKBox Churn prediction challenge
#predict whether a user will churn after his/her subscription expires. 
#Specifically, we want to forecast if a user make a new service subscription transaction within 30 days 
#after the current membership expiration date.

library(data.table)
library(dplyr)
library(plyr)

setwd("C:/Users/dtuiran/Documents/Machine Learning/Group Project/Data")

#Reading the data provided by Keegle

# Train: Subscription Expiration date in February, prediction for March
train<-fread("train.csv")

head(train)
#uniques id 992.931
length(unique(train$msno))

#Expiration for March, prediction for April
trainv2<-fread("train_v2.csv")

#uniques id 970.960
length(unique(trainv2$msno))

# Test data, predictions for April 2017 
test<-fread("sample_submission_zero.csv")

# Test data, predictions for April 2017 
test_v2<-fread("sample_submission_v2.csv")

# transactions of users up until 2/28/2017.
transactions<-fread("transactions.csv")
View(transactions[1:100,])


#2.363.626 uniques id
length(unique(transactions$msno))

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


##########################################


trant<-as.data.frame(transactions[1:5000,])
length(unique(trant$msno))

# Transactions preprosesing

#Convert to date 
transactions$transaction_date <- as.Date(as.character(transactions$transaction_date) ,format= "%Y%m%d" , origin = "19700101")
transactions$membership_expire_date <- as.Date(as.character(transactions$membership_expire_date) ,format= "%Y%m%d" , origin = "19700101")


#Creating new variable  pay_dif = Plan_list_price - actual_amount_paid
transactions$pay_dif <- transactions$plan_list_price- transactions$actual_amount_paid


#Max expire date per client , last date when day make transactions
require(plyr)
max_transdate<-ddply(transactions, .(msno), summarise, Value = max(transaction_date))

#LOS = Max(expire_date) - reg_initialdate 



## transactions / renewals = count the number of transactions where is_cancel = 0 
count_nocancel<-ddply(transactions, .(msno), summarise, Value = length(is_cancel == 0))
View(count_nocancel[1:100,])


# Mean pay diff by user
paydif<-ddply(transactions , .(msno), summarise, Value = mean(pay_dif))
#View(count_nocancel[1:100,])



# Create dummy variables for payment method
payment <- dcast(setDT(transactions),  msno ~  payment_method_id, fun.aggregate = any, value.var = "payment_method_id")
payment <- as.data.frame(mapply(payment, FUN=function(x) {if(is.logical(x)) as.numeric(x) else x }))
View(payment[1:50,])


View(transactions[transactions$msno == "///2pAPKetZe8zPqAwkYBjAUr+4pS8Rc6bsO4eGAlWI=",])
