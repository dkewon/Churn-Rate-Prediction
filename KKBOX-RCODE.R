
#Averages per variable, per user_id(msno) 
#Number of uses= count dates
#Max date  
#Uses per month  
#We can compare this date with expiration date.
#How many days without playing any song before the expiration date
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)
library(readr)

#reading and importing data
logs1 <- read_csv("user_logs.csv")
head(logs1)

View(logs1[1:100,])

#checking if user-ids are unique  # 1.649.247
length(unique(logs1$msno))
str(logs1)

#converting date format 
logs1$date<-as.Date(as.character(logs1$date), format="%Y%m%d")

#date<-data.frame(date)  
#logs1<-cbind(logs1,date)
#logs1[,2]=NULL

#subsetting data to check the codes

logs.test <-logs1[1:5000,]
length(unique(logs.test$msno))

#sample <- logs1[0.01 * row_number(logs1),]
#write.csv(MyData, file = "MyData.csv")

#getting average for each user
demo=aggregate(logs.test, by= list(logs.test$msno) ,FUN = mean)
demo$msno  <- NULL
colnames(demo)[1] <- "msno"
demo$date  <- NULL



