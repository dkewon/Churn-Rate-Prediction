#######Loading the basetable##############
library(readr)
basetable <- read_csv("basetable.csv")
View(basetable)

trainf <- read_csv("trainf.csv")
View(trainf)

testf <- read_csv("testf.csv")
View(testf)

sample_submission_v2 <- read_csv("sample_submission_v2.csv")
View(sample_submission_v2)

sum(trainf$is_churn)/length(trainf$is_churn) #0.06417505 6% churn
sum(testf$is_churn)/length(testf$is_churn) #0.06429461

###########Feature Selection##################

#####Lasso Regression (https://gerardnico.com/lang/r/ridge_lasso)
library(glmnet)
#x=model.matrix(is_churn~.,data=trainf) #Error: cannot allocate vector of size 1922.5 Gb
smallx<-head(trainf[,-1],1000)
x<-model.matrix(is_churn~.,data=smallx)
smally<-data.frame(trainf$is_churn)
y=head(smally,1000)
fit.lasso=glmnet(x,as.numeric(unlist(y)),alpha=1) # The larger is the parameter ?? the more number of coe???cients are shrinked to zero
plot(fit.lasso,xvar="lambda",label=TRUE) #Increasing lambda shrinks the coefficients


plot(fit.lasso,xvar="dev",label=TRUE) #The deviance shows the percentage of deviance

#Coefficient Extraction 
cv.lasso=cv.glmnet(x,as.numeric(unlist(y)))
plot(cv.lasso)
coef(cv.lasso) #pick the coefficient vector corresponding to the best model

# (Intercept)           0.095465527
# (Intercept)           .          
# is_cancel             0.195688618
# actual_amount_paid    .          
# is_auto_renew         .          
# autorenew_not_cancel -0.126858450
# notautorenew_cancel   .          
# amount_per_day        .          
# plan_days             0.001863589
# pay_dif               .          
# nbr_cancel            0.347469003
# nbr_transactions      .          
# During features selection process the variables that still have a non-zero coe???cient after the shrinking process are selected to be part of the model
# selected variables: is_cancel,autorenew_not_cancel,plan_days,nbr_cancel   

names(trainf)



#####Best Subset Selection
library(leaps)
regfit.full = regsubsets(is_churn ~ is_cancel+autorenew_not_cancel+plan_days+nbr_cancel+actual_amount_paid +is_auto_renew+notautorenew_cancel +amount_per_day+pay_dif+nbr_transactions+payment_method_id_41+payment_method_id_19+city+registered_via_13 +nbr_logs, data = trainf)
summary(regfit.full)

# Subset selection object
# Call: regsubsets.formula(is_churn ~ is_cancel + autorenew_not_cancel + 
#                            plan_days + nbr_cancel + actual_amount_paid + is_auto_renew + 
#                            notautorenew_cancel + amount_per_day + pay_dif + nbr_transactions + 
#                            payment_method_id_41 + payment_method_id_19 + city + registered_via_13 + 
#                            nbr_logs, data = trainf)
# 15 Variables  (and intercept)
# Forced in Forced out
# is_cancel                FALSE      FALSE
# autorenew_not_cancel     FALSE      FALSE
# plan_days                FALSE      FALSE
# nbr_cancel               FALSE      FALSE
# actual_amount_paid       FALSE      FALSE
# amount_per_day           FALSE      FALSE
# pay_dif                  FALSE      FALSE
# nbr_transactions         FALSE      FALSE
# payment_method_id_41     FALSE      FALSE
# payment_method_id_19     FALSE      FALSE
# city                     FALSE      FALSE
# registered_via_13        FALSE      FALSE
# nbr_logs                 FALSE      FALSE
# is_auto_renew            FALSE      FALSE
# notautorenew_cancel      FALSE      FALSE
# 1 subsets of each size up to 9
# Selection Algorithm: exhaustive
#         is_cancel autorenew_not_cancel plan_days nbr_cancel actual_amount_paid
# 1  ( 1 ) " "       " "                  "*"       " "        " "               
# 2  ( 1 ) " "       " "                  "*"       "*"        " "               
# 3  ( 1 ) " "       "*"                  "*"       "*"        " "               
# 4  ( 1 ) " "       "*"                  "*"       "*"        " "               
# 5  ( 1 ) "*"       "*"                  "*"       "*"        " "               
# 6  ( 1 ) " "       "*"                  "*"       "*"        " "               
# 7  ( 1 ) "*"       "*"                  "*"       "*"        "*"               
# 8  ( 1 ) " "       "*"                  "*"       "*"        " "               
# 9  ( 1 ) " "       "*"                  "*"       "*"        " "               
# is_auto_renew notautorenew_cancel amount_per_day pay_dif nbr_transactions
# 1  ( 1 ) " "           " "                 " "            " "     " "             
# 2  ( 1 ) " "           " "                 " "            " "     " "             
# 3  ( 1 ) " "           " "                 " "            " "     " "             
# 4  ( 1 ) " "           " "                 " "            "*"     " "             
# 5  ( 1 ) " "           " "                 " "            "*"     " "             
# 6  ( 1 ) "*"           " "                 " "            "*"     "*"             
# 7  ( 1 ) " "           " "                 " "            "*"     "*"             
# 8  ( 1 ) "*"           " "                 "*"            "*"     "*"             
# 9  ( 1 ) "*"           " "                 "*"            "*"     "*"             
# payment_method_id_41 payment_method_id_19 city registered_via_13 nbr_logs
# 1  ( 1 ) " "                  " "                  " "  " "               " "     
# 2  ( 1 ) " "                  " "                  " "  " "               " "     
# 3  ( 1 ) " "                  " "                  " "  " "               " "     
# 4  ( 1 ) " "                  " "                  " "  " "               " "     
# 5  ( 1 ) " "                  " "                  " "  " "               " "     
# 6  ( 1 ) " "                  " "                  " "  " "               " "     
# 7  ( 1 ) " "                  " "                  " "  " "               " "     
# 8  ( 1 ) "*"                  " "                  " "  " "               " "     
# 9  ( 1 ) "*"                  " "                  " "  " "               "*"   

#selected variables :autorenew_not_cancel+ plan_days+ nbr_cancel
###########Logistic Regression##################
###Logistic Regression based on lasso

model <- glm(is_churn ~ is_cancel+autorenew_not_cancel+plan_days+nbr_cancel,family=binomial(link='logit'),data=trainf)
summary(model)
library(pscl)
pR2(model) #accesing the model fit

#Prediction
fitted.results <- predict(model,newdata=testf,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

#Accuracy using mean
misClasificError <- mean(fitted.results != testf$is_churn)
print(paste('Accuracy',1-misClasificError))"Accuracy 0.960725949811898"

#Accuracy using AUC
library(ROCR)
library(pROC)
library(data.table)
library(mltools)
p <- predict(model, newdata=testf, type="response")
auc_roc(p, testf$is_churn)  # 0.856026


####Logistic Regression based on best subset selection

model <- glm(is_churn ~ autorenew_not_cancel+ plan_days+ nbr_cancel,family=binomial(link='logit'),data=trainf)
summary(model)

pR2(model) #accesing the model fit

#Prediction
fitted.results <- predict(model,newdata=testf,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

#Accuracy using mean
misClasificError <- mean(fitted.results != testf$is_churn)
print(paste('Accuracy',1-misClasificError))"Accuracy 0.96073054326806"

#Accuracy using AUC
p <- predict(model, newdata=testf, type="response")
auc_roc(p, testf$is_churn)  # 0.8561

