library(caret)
library(randomForest)
library(skimr)
library(tidyverse)
library(rpart)
library(FactoMineR)
library(gridExtra)
#library(pROC)

tr=read.csv('census_train.csv', header=T, strip.white=T)
#delete sample weights and education level
tr=tr[,-c(3,5)]
#missing is coded as "?"
skim(tr)
#change "?" to NA
tr[,c(2,5,12)]=as.data.frame(apply(tr[,c(2,5,12)], 2, function(x) replace(x, x %in% "?", NA)))

#numerical variables
p1=ggplot(tr, aes(x = income, y = age, fill = income)) + geom_boxplot()
p2=ggplot(tr, aes(x = income, y = hours.per.week, fill = income)) + geom_boxplot()
p3=ggplot(tr, aes(x = income, y = capital.gain, fill = income)) + geom_boxplot()
p4=ggplot(tr, aes(x = income, y = capital.loss, fill = income)) + geom_boxplot()
grid.arrange(p1,p2,p3,p4, ncol=2, nrow = 2)
#t tests
t.test(age~income, data=tr)
t.test(hours.per.week~income, data=tr)
t.test(capital.gain~income, data=tr)
t.test(capital.loss~income, data=tr)

#goup some factors that have multiple levels
#workclass
tr$workclass=fct_recode(tr$workclass,  unemp='Never-worked', unemp='Without-pay', gov='Federal-gov', gov='Local-gov', gov='State-gov')
summary(tr$workclass)
#occupation
tr$occupation=fct_recode(tr$occupation, skill='Craft-repair', skill='Machine-op-inspct', skill='Protective-serv',
             labor='Farming-fishing', labor='Handlers-cleaners', labor='Priv-house-serv', labor='Transport-moving',
             prof='Adm-clerical', prof='Exec-managerial', prof='Prof-specialty', prof='Sales', prof='Tech-support',
             other='Armed-Forces', other='Other-service')
summary(tr$occupation)
#education
tr$education=fct_recode(tr$education, preHS='Preschool', preHS='10th', preHS='11th', preHS='12th', 
                        preHS='1st-4th', preHS='5th-6th', preHS='7th-8th', preHS='9th',
                        assoc='Assoc-acdm', assoc='Assoc-voc')
summary(tr$education)
#marital status
tr$marital.status=fct_recode(tr$marital.status, married='Married-AF-spouse', married='Married-civ-spouse',
             alone='Married-spouse-absent', alone='Separated', alone='Widowed')
summary(tr$marital.status)
#race
tr$race=fct_recode(tr$race, minort='Amer-Indian-Eskimo', minort='Other')
summary(tr$race)
#relationship
table(tr$marital.status, tr$relationship)
tr$relationship=fct_recode(tr$relationship, nochild='Husband', nochild='Not-in-family', nochild='Other-relative', nochild='Unmarried', nochild='Wife')
summary(tr$relationship)

#examine the variable native.country
ggplot(tr, aes(x = native.country, y = ..count..)) +
  geom_bar(aes(fill = race), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#delete country variable
tr=tr[,-12]

#plots for factors with income
ggplot(tr, aes(x = workclass, y = ..count..)) +
  geom_bar(aes(fill = income), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(tr, aes(x = occupation, y = ..count..)) +
  geom_bar(aes(fill = income), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(tr, aes(x = marital.status, y = ..count..)) +
  geom_bar(aes(fill = income), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(tr, aes(x = sex, y = ..count..)) +
  geom_bar(aes(fill = income), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(tr, aes(x = race, y = ..count..)) +
  geom_bar(aes(fill = income), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#complete cases
trcmp=na.omit(tr)
(nrow(tr)-nrow(trcmp))/nrow(tr)

#Correspondance Analysis
tr.fct=trcmp[,c(2:8,12)]
mca1=MCA(tr.fct, graph = F)
cats = apply(tr.fct, 2, function(x) nlevels(as.factor(x)))
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)
# plot of variable categories
ggplot(data=mca1_vars_df,
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable), size=6) +
  ggtitle("MCA Plot of Categorical Variables")

##############random forest#############
#split into training and testing datasets
set.seed(621)
splt.cmp=createDataPartition(trcmp$income,p=0.7,list = F)
#training
trcmp1=trcmp[splt.cmp,]
#testing
trcmp2=trcmp[-splt.cmp,]

#define traincontrol
cntrl=trainControl(method="repeatedcv",number=10, repeats = 5, search="random")

ptm <- proc.time()
fit.cmp=train(x=trcmp1[,1:11],y=trcmp1$income, method="rf",trControl=cntrl,tuneLength=10)
proc.time() - ptm

print(fit.cmp)
plot(fit.cmp)
# result.predicted.prob <- predict(rf_model, df2, type="prob")
# result.roc <- roc(df2$income, result.predicted.prob$`<=50K`) # Draw ROC curve.
# plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
yhat.cmp=predict(fit.cmp, newdata = trcmp2)
confusionMatrix(data = yhat.cmp, trcmp2$income)
varImp(fit.cmp, scale=FALSE)
#mse
mean((as.numeric(yhat.cmp) - as.numeric(trcmp2$income))^2) #0.1345583



#impute
#impute workclass
imputwkcls=rpart(workclass ~ ., data = tr[!is.na(tr$workclass), ], method = "class")
tr$workclass[is.na(tr$workclass)]=predict(imputwkcls, tr[is.na(tr$workclass), ], type = "class")
summary(tr$workclass)
#impute occupation
imputoccp=rpart(occupation ~ ., data = tr[!is.na(tr$occupation), ], method = "class")
tr$occupation[is.na(tr$occupation)]=predict(imputoccp, tr[is.na(tr$occupation), ], type = "class")
summary(tr$occupation)

#split data
set.seed(621)
splt.tr=createDataPartition(tr$income,p=0.7,list = F)
#training
tr1=tr[splt.tr,]
#testing
tr2=tr[-splt.tr,]

ptm <- proc.time()
fit.imp=train(x=tr1[,1:11],y=tr1$income, method="rf",trControl=cntrl, tuneLength=10)
#fit.cmp=train(x=trcmp1[,1:11],y=trcmp1$income, method="rf",trControl=cntrl,tuneLength=10)
proc.time() - ptm

print(fit.imp)
plot(fit.imp)
yhat.imp=predict(fit.imp, newdata = tr2)
confusionMatrix(data = yhat.imp, tr2$income)
varImp(fit.imp, scale=FALSE)
#mse
mean((as.numeric(yhat.imp) - as.numeric(tr2$income))^2) #0.1280171




###############testing set###############
te=read.csv('census_test.csv', header=T, strip.white=T)
#delete sample weights and education level
te=te[,-c(3,5)]
#missing is coded as "?"
skim(te)
#change "?" to NA
te[,c(2,5,12)]=as.data.frame(apply(te[,c(2,5,12)], 2, function(x) replace(x, x %in% "?", NA)))

#goup some factors that have multiple levels
#workclass
te$workclass=fct_recode(te$workclass,  unemp='Never-worked', unemp='Without-pay', gov='Federal-gov', gov='Local-gov', gov='State-gov')
summary(te$workclass)
#occupation
te$occupation=fct_recode(te$occupation, skill='Craft-repair', skill='Machine-op-inspct', skill='Protective-serv',
                         labor='Farming-fishing', labor='Handlers-cleaners', labor='Priv-house-serv', labor='Transport-moving',
                         prof='Adm-clerical', prof='Exec-managerial', prof='Prof-specialty', prof='Sales', prof='Tech-support',
                         other='Armed-Forces', other='Other-service')
summary(te$occupation)
#education
te$education=fct_recode(te$education, preHS='Preschool', preHS='10th', preHS='11th', preHS='12th', 
                        preHS='1st-4th', preHS='5th-6th', preHS='7th-8th', preHS='9th',
                        assoc='Assoc-acdm', assoc='Assoc-voc')
summary(te$education)
#marital status
te$marital.status=fct_recode(te$marital.status, married='Married-AF-spouse', married='Married-civ-spouse',
                             alone='Married-spouse-absent', alone='Separated', alone='Widowed')
summary(te$marital.status)
#race
te$race=fct_recode(te$race, minort='Amer-Indian-Eskimo', minort='Other')
summary(te$race)
#relationship
table(te$marital.status, te$relationship)
te$relationship=fct_recode(te$relationship, nochild='Husband', nochild='Not-in-family', nochild='Other-relative', nochild='Unmarried', nochild='Wife')
summary(te$relationship)

#delete country variable
te=te[,-12]

#complete cases
tecmp=na.omit(te)
(nrow(te)-nrow(tecmp))/nrow(te)

#complete trained
yhat.cmp.new=predict(fit.cmp, newdata = tecmp)
confusionMatrix(data = yhat.cmp.new, tecmp$income)
#mse
mean((as.numeric(yhat.cmp.new) - as.numeric(tecmp$income))^2) # 0.1492495

#imputed trained
yhat.imp.new=predict(fit.imp, newdata = tecmp)
confusionMatrix(data = yhat.imp.new, tecmp$income)
#mse
mean((as.numeric(yhat.imp.new) - as.numeric(tecmp$income))^2) #0.1530369












###########a simple logistic regression############

fit.cmp.lg=glm(income~age+education+marital.status+occupation+race+sex+hours.per.week, data=trcmp1, family = "binomial")
step(fit.cmp.lg)  #same model
yhat.lg=predict(fit.cmp.lg, trcmp2, type = "response")
yhat.lg.bi=rep('<=50k', length(yhat.lg))
yhat.lg.bi[yhat.lg>=0.5]='>50k'
table(yhat.lg.bi, trcmp2$income)
5862/7075























######################without aggressive grouping############################
tr=read.csv('census_train.csv', header=T, strip.white=T)
#delete sample weights and education level
tr=tr[,-c(3,5)]
#change "?" to NA
tr[,c(2,5,12)]=as.data.frame(apply(tr[,c(2,5,12)], 2, function(x) replace(x, x %in% "?", NA)))


#goup some factors that have multiple levels
#workclass
tr$workclass=fct_recode(tr$workclass,  unemp='Never-worked', unemp='Without-pay', gov='Federal-gov', gov='Local-gov', gov='State-gov')
summary(tr$workclass)
#occupation
summary(tr$occupation)
#education
tr$education=fct_recode(tr$education, preHS='Preschool', preHS='10th', preHS='11th', preHS='12th', 
                        preHS='1st-4th', preHS='5th-6th', preHS='7th-8th', preHS='9th',
                        assoc='Assoc-acdm', assoc='Assoc-voc')
summary(tr$education)
#marital status
tr$marital.status=fct_recode(tr$marital.status, married='Married-AF-spouse', married='Married-civ-spouse')
summary(tr$marital.status)
#race
tr$race=fct_recode(tr$race, minort='Amer-Indian-Eskimo', minort='Other')
summary(tr$race)
#relationship
tr$relationship=fct_recode(tr$relationship, nochild='Husband', nochild='Not-in-family', nochild='Other-relative', nochild='Unmarried', nochild='Wife')
summary(tr$relationship)
#delete country variable
tr=tr[,-12]


#complete cases
trcmp=na.omit(tr)
(nrow(tr)-nrow(trcmp))/nrow(tr)

##############random forest#############
#split into training and testing datasets
set.seed(621)
splt.cmp=createDataPartition(trcmp$income,p=0.7,list = F)
#training
trcmp1=trcmp[splt.cmp,]
#testing
trcmp2=trcmp[-splt.cmp,]

#define traincontrol
cntrl=trainControl(method="repeatedcv",number=10, repeats = 5, search="random")

ptm <- proc.time()
fit.cmp=train(x=trcmp1[,1:11],y=trcmp1$income, method="rf",trControl=cntrl,tuneLength=10)
proc.time() - ptm

print(fit.cmp)
plot(fit.cmp)

yhat.cmp=predict(fit.cmp, newdata = trcmp2)
confusionMatrix(data = yhat.cmp, trcmp2$income)     #0.8625 
varImp(fit.cmp, scale=FALSE)
#mse
mean((as.numeric(yhat.cmp) - as.numeric(trcmp2$income))^2) #0.1375265



#impute
#impute workclass
imputwkcls=rpart(workclass ~ ., data = tr[!is.na(tr$workclass), ], method = "class")
tr$workclass[is.na(tr$workclass)]=predict(imputwkcls, tr[is.na(tr$workclass), ], type = "class")
summary(tr$workclass)
#impute occupation
imputoccp=rpart(occupation ~ ., data = tr[!is.na(tr$occupation), ], method = "class")
tr$occupation[is.na(tr$occupation)]=predict(imputoccp, tr[is.na(tr$occupation), ], type = "class")
summary(tr$occupation)

#split data
set.seed(621)
splt.tr=createDataPartition(tr$income,p=0.7,list = F)
#training
tr1=tr[splt.tr,]
#testing
tr2=tr[-splt.tr,]

ptm <- proc.time()
fit.imp=train(x=tr1[,1:11],y=tr1$income, method="rf",trControl=cntrl, tuneLength=10)
proc.time() - ptm

print(fit.imp)
plot(fit.imp)
yhat.imp=predict(fit.imp, newdata = tr2)
confusionMatrix(data = yhat.imp, tr2$income) #0.8697
varImp(fit.imp, scale=FALSE)
#mse
mean((as.numeric(yhat.imp) - as.numeric(tr2$income))^2) # 0.130284




###############testing set###############
te=read.csv('census_test.csv', header=T, strip.white=T)
#delete sample weights and education level
te=te[,-c(3,5)]
#change "?" to NA
te[,c(2,5,12)]=as.data.frame(apply(te[,c(2,5,12)], 2, function(x) replace(x, x %in% "?", NA)))

#goup some factors that have multiple levels
#workclass
te$workclass=fct_recode(te$workclass,  unemp='Never-worked', unemp='Without-pay', gov='Federal-gov', gov='Local-gov', gov='State-gov')
summary(te$workclass)
#occupation
summary(te$occupation)
#education
te$education=fct_recode(te$education, preHS='Preschool', preHS='10th', preHS='11th', preHS='12th', 
                        preHS='1st-4th', preHS='5th-6th', preHS='7th-8th', preHS='9th',
                        assoc='Assoc-acdm', assoc='Assoc-voc')
summary(te$education)
#marital status
te$marital.status=fct_recode(te$marital.status, married='Married-AF-spouse', married='Married-civ-spouse')
summary(te$marital.status)
#race
te$race=fct_recode(te$race, minort='Amer-Indian-Eskimo', minort='Other')
summary(te$race)
#relationship
te$relationship=fct_recode(te$relationship, nochild='Husband', nochild='Not-in-family', nochild='Other-relative', nochild='Unmarried', nochild='Wife')
summary(te$relationship)
#delete country variable
te=te[,-12]

#complete cases
tecmp=na.omit(te)
(nrow(te)-nrow(tecmp))/nrow(te)

#complete trained
yhat.cmp.new=predict(fit.cmp, newdata = tecmp)
confusionMatrix(data = yhat.cmp.new, tecmp$income)        #0.8472
#mse
mean((as.numeric(yhat.cmp.new) - as.numeric(tecmp$income))^2) # 0.1527563

#imputed trained
yhat.imp.new=predict(fit.imp, newdata = tecmp)
confusionMatrix(data = yhat.imp.new, tecmp$income)        #0.8449
#mse
mean((as.numeric(yhat.imp.new) - as.numeric(tecmp$income))^2) #0.155141




























