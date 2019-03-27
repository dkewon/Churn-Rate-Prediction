library(readr)
members_v3 <- read_csv("C:\\Users\\dkewon\\Documents\\Kaggle\\members_v3.csv")
View(members_v3)

train_v2 <- read_csv("C:\\Users\\dkewon\\Documents\\Kaggle\\train_v2.csv")
View(train_v2)

transactions_v2 <- read_csv("C:\\Users\\dkewon\\Documents\\Kaggle\\transactions_v2.csv")
View(transactions_v2)

user_logs_v2 <- read_csv("C:\\Users\\dkewon\\Documents\\Kaggle\\user_logs_v2.csv")
View(user_logs_v2)

# Missing values,outliers and duplicates
# City - Dummy
# BD (AGE) -Outliers
# Gender -missing values, Dummy or no
# Register_via -1 value
# Regis_init-convert to date? #registration_init_time in 1970?



###########################Users####################################
#to verify if there are duplicates
duporno<-data.frame(table(members_v3$msno))
members<-members_v3[!duplicated(members_v3$msno), ] #no duplicates
#missing values
colSums(is.na(members_v3)) # only gender-4429505

##########################City######################################## 
options(scipen=999)
cityt<-data.frame(table(members_v3$city))
#city frequency
ggplot(cityt, aes(x = as.character(cityt$Var1), y = cityt$Freq)) + geom_bar(stat="identity") +
   labs(y = "Frequency", x='City')
#most users from city1. Would it be still useful to create dummies?
#members<-dummy(members_v3$city, sep = ".")
#members<-data.frame(members)

#turning all other cities into 0
members_v3$city[members_v3$city!=1] <- 0

##############################Age###################################
#age frequency
aget<-data.frame(table(members_v3$bd)) #-7168 to 2016 very messy
#age_sub<-subset(members_v3, bd>=0 & bd<=120)
options(scipen=999)
plot(aget$Freq)
boxplot(Freq~Var1,data=aget)
summary(aget)

#replace odd values with 0
members_v3$bd[members_v3$bd>=80|members_v3$bd<10] <- 0 #age range 10 to 80 and 0 as invalid values 
hist(members_v3$bd) #compare how age influences churn=0 | churn=1
##############################Gender###################################
#male 1 and female 2
members_v3$gender[members_v3$gender=="male"] <- 1 
members_v3$gender[members_v3$gender=="female"] <- 2 
# since more than half of the users didn't identify their genders, it will be very hard to replace them with numbers;keeping them as they are 
members_v3$gender[is.na(members_v3$gender)] <- 0

#making dummies for male and female (# of dum= 3-1=2)
resultsgen <- fastDummies::dummy_cols(members_v3, select_columns = "gender")
resultsgen[,2:7] <- NULL
members_v3<-merge(members_v3, resultsgen, by.x = "msno", by.y = "msno")

################################# Register_via######################
registration<-data.frame(table(members_v3$registered_via))
ggplot(registration, aes(x = as.character(registration$Var1), y = registration$Freq)) + geom_bar(stat="identity") +
  labs(y = "Frequency", x='Registration Platform')
# there is one -1. It was replaced with mode 4
members_v3$registered_via[members_v3$registered_via==-1] <- 4 
# creating dummies 
results <- fastDummies::dummy_cols(members_v3, select_columns = "registered_via")
results[,2:6] <- NULL
members_v3<-merge(members_v3, results, by.x = "msno", by.y = "msno")

#################################### Regis_init##################################
table(members_v3$registration_init_time) #20040326 to 20170429
#Changing numeric into date (ex.20160101 into 2016-01-01)
regis_init_time<-(as.Date(as.character(members_v3$registration_init_time), format="%Y%m%d"))
regis_init_time<-data.frame(regis_init_time)  
members_v3<-cbind(members_v3,regis_init_time)
