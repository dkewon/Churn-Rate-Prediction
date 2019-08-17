######### basetable #########
library(readr)
train_basetable <- read_csv("Kaggle/train_basetable.csv")

setwd("C:\\Users\\dkewon\\Documents\\Kaggle")

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
# 0      1 
# 929460  63471
table(test$is_churn)
# 0 
# 970960
info<-read.csv("train_basetable.csv")
info$X <-NULL

#Adding the other parameters
train<-merge(x = train,y=info, all.x = TRUE, by="msno")
train<-train[complete.cases(train), ]

#Adding the other parameters
test<-merge(x = test,y=info, all.x = TRUE, by="msno")
test<-test[complete.cases(test), ]

######### WoE/Logistic Regression#########

#WoE
library(devtools)
library(woe)
library(information)

#Calculating the Information Value
options(scipen=999)
IV <- create_infotables(data=train, y="is_churn", bins=10, parallel=FALSE) # parallel=FALSE is to exclude the target 
IV_Value = data.frame(IV$Summary)
#top 6 IV
# renewals
# 0.913944647578931

# registered_via_7
# 1.116765189951893

# actual_amount_paid
# 1.680616592434047

# is_auto_renew
# 2.468010865338544

# autorenew_not_cancel
# 2.817950970739108

# nbr_transactions
# 0.779018754913990

#Logistic Regression
model <- glm(is_churn ~ renewals + registered_via_7 + actual_amount_paid + is_auto_renew + autorenew_not_cancel+nbr_transactions,family=binomial(link='logit'),data=train)
summary(model)
# Call:
#   glm(formula = is_churn ~ renewals + registered_via_7 + actual_amount_paid + 
#         is_auto_renew + autorenew_not_cancel + nbr_transactions, 
#       family = binomial(link = "logit"), data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -4.2406  -0.1205  -0.0915  -0.0727   5.0341  
# 
# Coefficients:
#   Estimate  Std. Error z value            Pr(>|z|)    
# (Intercept)          -1.34124216  0.01669265  -80.35 <0.0000000000000002 ***
#   renewals             -0.61782932  0.01211643  -50.99 <0.0000000000000002 ***
#   registered_via_7     -0.86882166  0.02789556  -31.15 <0.0000000000000002 ***
#   actual_amount_paid    0.00073326  0.00001676   43.75 <0.0000000000000002 ***
#   is_auto_renew        -0.73071478  0.03571541  -20.46 <0.0000000000000002 ***
#   autorenew_not_cancel -2.37453855  0.03734852  -63.58 <0.0000000000000002 ***
#   nbr_transactions      0.57139456  0.01168554   48.90 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 195257  on 739497  degrees of freedom
# Residual deviance: 128457  on 739491  degrees of freedom
# AIC: 128471
# 
# Number of Fisher Scoring iterations: 8

anova(model, test="Chisq") # significant variables:renewals+registered_via_7+actual_amount_paid+autorenew_not_cancel+nbr_transactions

install.packages('pscl')
library(pscl)
pR2(model) #accesing the model fit

#Prediction
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

#Accuracy using mean
misClasificError <- mean(fitted.results != test$is_churn)
print(paste('Accuracy',1-misClasificError)) #Accuracy 0.9994

#Accuracy using AUC
install.packages('ROCR')
library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$is_churn) # this is not working as test target only includes 0s
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]] 

