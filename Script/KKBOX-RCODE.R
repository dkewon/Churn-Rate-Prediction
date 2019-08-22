
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

# song listening %
less_than_50<-rowMeans(demo[c('num_25', 'num_50')], na.rm=TRUE)
more_than_50<-rowMeans(demo[c('num_75', 'num_985')], na.rm=TRUE)
more_than_98<-rowMeans(demo[c('num_100', 'num_unq')], na.rm=TRUE)
  
demo<- cbind(demo, less_than_50)
demo<- cbind(demo, more_than_50)
demo<- cbind(demo,more_than_98)
demo$num_25  <- NULL
demo$num_50  <- NULL
demo$num_75  <- NULL
demo$num_985  <- NULL
demo$num_100  <- NULL
demo$num_unq  <- NULL

colnames(demo)[4]<-"between_50_98"

