####################################
#### TRY

# Clasiffication: KKBox Churn prediction challenge
#predict whether a user will churn or not after his/her subscription expires. Will a customer re-subscribe within 30 days of expiration?



########################## Installing necessary packages ####################
install.packages("data.table")
install.packages("dplyr")
install.packages("plyr")
install.packages("yaml")

library(data.table)
library(dplyr)
library(plyr)
library(fastDummies)

#Define working directory
setwd("C:/Users/dkewon/Documents/Machine Learning/Group Project/Data")

##########################Reading the data ############################################################

# Train: Subscription Expiration date in February, prediction for March
train<-fread("train.csv")

#uniques id 992.931
#length(unique(train$msno))

#Expiration for March, prediction for April
trainv2<-fread("train_v2.csv")

#uniques id 970.960
length(unique(trainv2$msno))

# Test data, predictions for April 2017 
test<-fread("sample_submission_zero.csv")

# Test data, 
#test_v2<-fread("sample_submission_v2.csv")

# transactions of users up until 2/28/2017.
transactions1<-fread("transactions.csv")
#View(transactions[1:100,])

# transactions data until 3/31/2017.
transactions2<-fread("transactions_v2.csv")
#View(transactions2[1:100,])


transactions<-rbind.data.frame(transactions1,transactions2)


#daily user logs describing listening behavior of a user. 
#Data collected until 2/28/2017.
user_logs1<-fread("user_logs.csv")
#Unique id 2.140.938
#length(unique(user_logs$msno))


#contains the user logs data until 3/31/2017.
user_logs2<-fread("user_logs_v2.csv")
#View(user_logs[1:100,])

user_logs<-rbind.data.frame(user_logs1,user_logs2)


#user information, with out exp date from members 
members<-fread("members_v3.csv")
#View(members[1:100,])



##########################Users_logs####################
#Uses_logs preprocessing


user_logs.new <-user_logs[ , .(nbr_logs = length(date),
                           last_log = max(date),
                           num_25.mean = mean(num_25),
                           num_50.mean = mean(num_50),
                           num_75.mean = mean(num_75),
                           num_985.mean = mean(num_985),
                           num_100.mean = mean(num_100),
                           num_uniques = mean(num_unq),
                           total_sec.mean = mean(total_secs)),by = .(msno)]

user_logs.new$last_log <- as.Date(as.character(user_logs.new$last_log) ,format= "%Y%m%d" , origin = "19700101")

#write.csv(user_logs.new, file= "user_logs.new.csv")


##########################Transactions########################################################################
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
#Binary feature to predict churning if auto_renew = 0 and is_cancel = 1 

transactions$notautorenew_cancel<-ifelse((transactions$is_auto_renew == 0) & 
                                            (transactions$is_cancel == 1) , 1, 0)

#Creating new variable  pay_dif = Plan_list_price - actual_amount_paid
transactions$pay_dif <- transactions$plan_list_price- transactions$actual_amount_paid

#Get last observation for msno","is_auto_renew","autorenew_not_cancel","notautorenew_cancel by Id(msno)
transactions.unique<-transactions[,c("msno","actual_amount_paid", "payment_plan_days","is_auto_renew","autorenew_not_cancel","notautorenew_cancel")]
transactions.unique<-transactions.unique[, .SD[.N], by =msno]


#Amount pay per day 
transactions.unique$amount_per_day<-transactions.unique$actual_amount_paid / transactions.unique$payment_plan_days
transactions.unique$amount_per_day<-ifelse(is.infinite(transactions.unique$amount_per_day),transactions.unique$amount_per_day/ Inf , transactions.unique$amount_per_day)
transactions.unique$payment_plan_days<-NULL


#Max expire date per client 
#Max expire date
#Min expire date per client 
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
#install.packages("fastDummies")

payment_method<-transactions[,c("msno","payment_method_id")]
payment_method<-payment_method[, .SD[.N], by =msno]
payment_method<-dummy_cols(payment_method, select_columns = "payment_method_id", remove_first_dummy = TRUE)
payment_method$payment_method_id<- NULL

#Merging all together
transactions.final<-merge(x = transactions.unique,y=transactions_new, by="msno")
transactions.final<-merge(x= transactions.final, y= payment_method, by="msno")

#write.csv(transactions.final,file = "transactions_final.csv")

##########################Members############################################################################################
#Members preprocessing
length(unique(members$msno))
View(members[1:1000,])


##################Users
#to verify if there are duplicates
#duporno<-data.frame(table(members_v3$msno))
#members<-members_v3[!duplicated(members_v3$msno), ] #no duplicates
#missing values
#colSums(is.na(members_v3)) # only gender-4429505

##########################City
options(scipen=999)
cityt<-data.frame(table(members$city))
#city frequency
ggplot(cityt, aes(x = as.character(cityt$Var1), y = cityt$Freq)) + geom_bar(stat="identity") +
  labs(y = "Frequency", x='City')
#most users from city1. Would it be still useful to create dummies?
#members<-dummy(members_v3$city, sep = ".")
#members<-data.frame(members)

#turning all other cities into 0
members$city[members$city!=1] <- 0


#Age
#age frequency
aget<-data.frame(table(members$bd)) #-7168 to 2016 very messy
#age_sub<-subset(members_v3, bd>=0 & bd<=120)
options(scipen=999)
plot(aget$Freq)
boxplot(Freq~Var1,data=aget)
summary(aget)

#replace odd values with 0
members$bd[members$bd>=80|members$bd<10] <- 0 #age range 10 to 80 and 0 as invalid values 
hist(members$bd) #compare how age influences churn=0 | churn=1


#Gender
#male 1 and female 2
members$gender[members$gender=="male"] <- 1 
members$gender[members$gender=="female"] <- 2 
# since more than half of the users didn't identify their genders, it will be very hard to replace them with numbers;keeping them as they are 
members$gender[members$gender == ""] <- 0


#making dummies for male and female (# of dum= 3-1=2)
fastDummies::dummy_cols(members, select_columns = "gender", remove_first_dummy = TRUE)
#resultsgen[,2:6] <- NULL
#members<-merge(members, resultsgen, by = "msno")

# Register_via
registration<-data.frame(table(members$registered_via))
ggplot(registration, aes(x = as.character(registration$Var1), y = registration$Freq)) + geom_bar(stat="identity") +
  labs(y = "Frequency", x='Registration Platform')
# there is one -1. It was replaced with mode 4
members$registered_via[members$registered_via==-1] <- 4 
# creating dummies 
fastDummies::dummy_cols(members, select_columns = "registered_via",remove_first_dummy = TRUE)
#results[,2:10] <- NULL
#members<-merge(members, results, by.x = "msno", by.y = "msno")


# Regis_init
table(members$registration_init_time) #20040326 to 20170429

#Changing numeric into date (ex.20160101 into 2016-01-01)
members$registration_init_time<-(as.Date(as.character(members$registration_init_time), format="%Y%m%d"))

#members<-cbind(members,regis_init_time)


##########################Merge_all###################


train_basetable<-merge(transactions.final,members,by ="msno", all.x = TRUE)
train_basetable<-merge(train_basetable,user_logs.new, by= "msno",all.x = TRUE)

#Deleting users with transactions dates in 1970
train_basetable<-train_basetable[!(train_basetable$max_expire == "1970-01-01"),]

nan_count<-as.data.frame(sapply(train_basetable, function(x) sum(is.na(x))))

#Delete columns with out complete info
train_basetable<-train_basetable[complete.cases(train_basetable), ]


##########################Create new variables #######################################
#LOS =Max(expire_date) - reg_initialdate 
train_basetable$LOS <- train_basetable$max_expire - train_basetable$registration_init_time
hist(as.numeric(train_basetable$LOS))

#Number of days the user takes to start using the service, since the initial registration date
train_basetable$start.time <- train_basetable$min_transdate - train_basetable$registration_init_time
hist(as.numeric(train_basetable$start.time))
train_basetable<-train_basetable[!(train_basetable$start.time < 0),]

#Number of renewals
train_basetable$renewals <- train_basetable$nbr_transactions - train_basetable$nbr_cancel
hist(as.numeric(train_basetable$renewals))

#remove variables not used for predictions
train_basetable$max_transdate <- NULL
train_basetable$min_transdate <- NULL
train_basetable$max_expire <- NULL
train_basetable$registered_via <- NULL
train_basetable$registered_via <-NULL
train_basetable$registration_init_time <- NULL
train_basetable$last_log <- NULL

write.csv(train_basetable, "train_basetable.csv")

######################### Train data set ####################### 
