

# Clasiffication: KKBox Churn prediction challenge
#predict whether a user will churn after his/her subscription expires. 
#Specifically, we want to forecast if a user make a new service subscription transaction within 30 days 
#after the current membership expiration date.

install.packages("data.table")
install.packages("dplyr")
install.packages("plyr")
install.packages("yaml")

library(data.table)
library(dplyr)
library(plyr)

#Define working directory
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
#transactions2<-fread("transactions_v2.csv")
#View(transactions2[1:100,])

#daily user logs describing listening behaviors of a user. 
#Data collected until 2/28/2017.
user_logs<-fread("user_logs.csv")
user_logs<-user_logs[order(msno),]

#contains the user logs data until 3/31/2017.
#user_logs2<-fread("user_logs_v2.csv")
#View(user_logs[1:100,])

#user information, with out exp date from members 
#members<-fread("members_v3.csv")
#View(members[1:100,])


##########################################

#Subset of data 
trant<-as.data.frame(transactions[1:5000,])
length(unique(trant$msno))

# Transactions preprosesing

#payment method
hist(transactions$payment_method_id)

#Plan days, mean 31 days.
hist(transactions$payment_plan_days)
as.data.frame(table(transactions$payment_plan_days))
mean(transactions$payment_plan_days)

#is_auto_renew
as.data.frame(table(transactions$is_auto_renew))

#is_auto_cancel
as.data.frame(table(transactions$is_cancel))

#Convert to date 
transactions$transaction_date <- as.Date(as.character(transactions$transaction_date) ,format= "%Y%m%d" , origin = "19700101")
transactions$membership_expire_date <- as.Date(as.character(transactions$membership_expire_date) ,format= "%Y%m%d" , origin = "19700101")


#autorenew_&_not_cancel
transactions$autorenew_not_cancel<-ifelse((transactions$is_auto_renew == 1) & 
                                              (transactions$is_cancel == 0) , 1, 0)


#notAutorenew_&_cancel 
#Binary feature to predict possible churning if auto_renew = 0 and is_cancel = 1 

transactions$notautorenew_cancel<-ifelse((transactions$is_auto_renew == 0) & 
                                            (transactions$is_cancel == 1) , 1, 0)


#Creating new variable  pay_dif = Plan_list_price - actual_amount_paid
transactions$pay_dif <- transactions$plan_list_price- transactions$actual_amount_paid


View(transactions[1:1000,])


#Get last observation for msno","is_auto_renew","autorenew_not_cancel","notautorenew_cancel by Id(msno)
transactions.unique<-transactions[,c("msno","actual_amount_paid", "payment_plan_days","is_auto_renew","autorenew_not_cancel","notautorenew_cancel")]
transactions.unique<-transactions.unique[, .SD[.N], by =msno]


#Amount pay per day 
transactions.unique$amount_per_day<-transactions.unique$actual_amount_paid / transactions.unique$payment_plan_days
transactions.unique$amount_per_day<-ifelse(is.infinite(transactions.unique$amount_per_day),transactions.unique$amount_per_day/ Inf , transactions.unique$amount_per_day)

transactions.unique$payment_plan_days<-NULL


#Max expire date per client , last date when day make transactions
#Max expire date
#Min expire date per client , first date when day make transactions
# Mean pay diff by user
#Total number of transactions
#Number of cancelations


transactions_new<- transactions[ , .(
                        max_transdate = max(transaction_date), 
                        max_expire = max(membership_expire_date), 
                        min_transdate = min(transaction_date),
                        plan_days = mean(payment_plan_days),
                        pay_dif = mean(pay_dif),
                        nbr_cancel =sum(is_cancel),
                        nbr_transactions = length(is_cancel)),by = .(msno)]


#Any discount applied
transactions_new$is_discount <- ifelse(transactions_new$pay_dif > 0, 1 ,0)


# Create dummy variables for payment method

install.packages("fastDummies")
library("fastDummies")


payment_method<-transactions[,c("msno","payment_method_id")]
payment_method<-payment_method[, .SD[.N], by =msno]
payment_method<-dummy_cols(payment_method, select_columns = "payment_method_id", remove_first_dummy = TRUE)
payment_method$payment_method_id<- NULL

#Merging all together
transactions.final<-merge(x = transactions.unique,y=transactions_new, by="msno")
transactions.final<-merge(x= transactions.final, y= payment_method, by="msno")


write.csv(transactions.final,file = "transactions_final.csv")


View(transactions[transactions$msno =="UkDFI97Qb6+s2LWcijVVv4rMAsORbVDT2wNXF0aVbns=",])
View(transactions.final[transactions.final$msno =="UkDFI97Qb6+s2LWcijVVv4rMAsORbVDT2wNXF0aVbns=",])


#LOS = Max(expire_date) - reg_initialdate 