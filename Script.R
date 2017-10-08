# There are 2 datasets - "bank" and "bank-additional".
# The "bank-additional" version has fewer records, but more measures, so select to use this one?  
# No balance, fewer defaults, but more variables in the bank-additional version  

setwd("C:/Users/Alvin/Downloads/SAS BDWP")

# Read in data.
bankdata<-read.csv("bank-additional-full.csv",sep=";",header=TRUE)

# Load packages
if(!require("caret")){install.packages("caret"); library(caret)}
if(!require("randomForest")){install.packages("randomForest"); library(randomForest)}
if(!require("pROC")){install.packages("pROC"); library(pROC)}


# Split training and cv, so we get 60:20:20 proportion
set.seed(0816)
bankdata$rand<-runif(nrow(bankdata),0,1)
bankdata$id<-"train"
bankdata$id[bankdata$rand>0.75]<-"cv"

# Override data so that 20% is "in the future"
bankdata[(0.8*nrow(bankdata)):(nrow(bankdata)),"id"]<-"test"

# Create train dataset for exploratory analyses
bankdata_train<-bankdata[bankdata$id=="train",]

# Exploratory analyses - some examples

##  barplots

## Age
par(mar = c(5,5,4,4))    
hist(bankdata$age,main="Distribution of Customers by Age",xlab="Age in Years",col="#BB8FCE")
par(new=T)
plot(prop.table(table(bankdata$age,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col="#CB4335", pch = 16)
par(new=T)
mtext("Probability of Conversion", side=4, line=3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## Job
par(mar = c(7,5,4,4))
plot(bankdata$job, main="Distribution of Customers by Job", ylab = "Frequency", las = 2, col = "turquoise3", xlim = c(0,15))
par(new=T)
plot(prop.table(table(bankdata$job,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "orangered3", pch = 16, xlim = c(0.5,13))
mtext("Probability of Conversion",side=4, line = 3)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

## Day of Week
bankdata$day_of_week<-factor(bankdata$day_of_week, c("mon","tue","wed","thu","fri"))
par(mar = c(5,5,5,5))
plot(bankdata$day_of_week, main="Distribution of Calls by Day of Week", ylab = "Frequency", las = 2, col = "steelblue2", xlim = c(0,6))
par(new=T)
plot(prop.table(table(bankdata$day_of_week,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col = "red4", pch = 16, lwd = 2, xlim = c(0.4,5.4))
mtext("Probability of Conversion",side=4, line = 3.5)
axis(4, ylim=c(0,1), col="black",col.axis="black",las=1)

# Feature Engineering
## AGE
hist(bankdata$age,main="Age",xlab=NULL,col="firebrick")
par(new=T)
plot(prop.table(table(bankdata$age,bankdata$y),1)[,1],xlab=NA,ylab=NA, axes=F)
##regroup ages into 0-25, 25-40, 41-55, >55 based on charts
bankdata$agegroup<-"Under25"
bankdata$agegroup[bankdata$age<41 & bankdata$age>24]<-"b25to40"
bankdata$agegroup[bankdata$age<56 & bankdata$age>40]<-"b41to55"
bankdata$agegroup[bankdata$age>55]<-"Above55"

## DURATION - need to be excluded
bankdata$durationgroup<-floor(bankdata$duration/30)
table(bankdata$durationgroup)
#decide to group >3 years together
bankdata$durationgroup[bankdata$durationgroup>35]<-36
hist(bankdata$durationgroup,main="Duration",xlab=NULL,col="firebrick")
par(new=T)
plot(prop.table(table(bankdata$durationgroup,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F, col="blue")

# EURIBOR - round to nearest %
bankdata$euriborgroup<-round(bankdata$euribor3m/0.5)*0.5
bankdata$euriborgroup[bankdata$euriborgroup==2]<-1.5
bankdata$euriborgroup[bankdata$euriborgroup==3]<-4
bankdata$euriborgroup[bankdata$euriborgroup==3.5]<-4
bankdata$euriborgroup[bankdata$euriborgroup==4.5]<-4
hist(bankdata$euriborgroup,main="Euribor",xlab=NULL,col="firebrick")
par(new=T)
plot(prop.table(table(bankdata$euriborgroup,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F)

# CAMPAIGN - plot shows it appears to be ascending in sequence
# Group all above 15 together
bankdata$campaigngroup<-bankdata$campaign
bankdata$campaigngroup[bankdata$campaign>15]<-16
hist(bankdata$campaigngroup,main="Campaign",xlab=NULL,col="blue")
par(new=T)
plot(prop.table(table(bankdata$campaigngroup,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F)

# PDAYS - convert to a binary factor, yes or no.

bankdata$contact_in_last_month<-"Yes"
bankdata$contact_in_last_month[bankdata$pdays==999]<-"No"
plot(table(bankdata$contact_in_last_month),main="Contacted in last month?",xlab=NULL,col="blue")
par(new=T)
plot(prop.table(table(bankdata$contact_in_last_month,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F)

#Group up previous
bankdata$previousgroup<-bankdata$previous
bankdata$previousgroup[bankdata$previous>3]<-3
hist(bankdata$previousgroup,main="Previous",xlab=NULL,col="blue")
par(new=T)
plot(prop.table(table(bankdata$previousgroup,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F)

#Group up cons.idx
bankdata$conspricegroup<-round(bankdata$cons.price.idx/0.2)*0.2
bankdata$consconfgroup<-round(bankdata$cons.conf.idx/0.5)*0.5
hist(bankdata$conspricegroup,main="Consprice",xlab=NULL,col="blue")
par(new=T)
plot(prop.table(table(bankdata$conspricegroup,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F)
hist(bankdata$consconfgroup,main="ConsConf",xlab=NULL,col="blue")
par(new=T)
plot(prop.table(table(bankdata$consconfgroup,bankdata$y),1)[,2],xlab=NA,ylab=NA, axes=F)

# MODELLING
#Convert to factors, and change job levels
bankdata$job<-as.character(bankdata$job)
bankdata$job[bankdata$job=="blue-collar"]<-"blue.collar"
bankdata$job[bankdata$job=="self-employed"]<-"self.employed"
bankdata$job[is.na(bankdata$job)]<-"unknown"
# bankdata[sapply(bankdata,is.numeric)]<-lapply(bankdata[sapply(bankdata,is.numeric)],as.factor)
bankdata[sapply(bankdata,is.character)]<-lapply(bankdata[sapply(bankdata,is.character)],as.factor)
bankdata$y<-as.factor(bankdata$y)

#Create train, cv and test datasets
train<-bankdata[bankdata$id=="train",]
cv<-bankdata[bankdata$id=="cv",]
test<-bankdata[bankdata$id=="test",]

# GLM fit

set.seed(0817)

glmfit <- glm(y~job + marital + education + default + housing + loan + contact + day_of_week 
                + campaigngroup + contact_in_last_month + previousgroup 
                + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup 
                + nr.employed + agegroup,
                data=train, family=binomial)
glmpred <- predict(glmfit, newdata=cv, type="response")
glmroc <- roc(cv$y, glmpred)
glmauc <- glmroc$auc

glmpred_test <- predict(glmfit, newdata=test, type="response")
glmroc_test <- roc(test$y, glmpred_test)
glmauc_test <- glmroc_test$auc

# Other fits
# GLM boost
fitControl <- trainControl(method = "repeatedcv",
                            number = 5,
                            repeats = 10,
                            classProbs = TRUE)
 
glmBoostfit<-train(y~job + marital + education + default + housing + loan + contact + day_of_week
                     + campaigngroup + contact_in_last_month + previousgroup
                     + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup
                     + nr.employed + agegroup,
                     data=train, method="glmboost",
                     metric = "ROC",
                     trControl = fitControl,
                     family=Binomial(link=c("logit")),
                     tuneLength=5)

glmBoostpred<-predict(glmBoostfit, newdata=cv, type="prob")[,"yes"]
glmBoostroc<-roc(cv$y, glmBoostpred)
glmBoostauc<-glmBoostroc$auc

glmBoostpred_test<-predict(glmBoostfit, newdata=test, type="prob")[,"yes"]
glmBoostroc_test<-roc(test$y, glmBoostpred_test)
glmBoostauc_test<-glmBoostroc_test$auc

## Fit CART

cartfit<-train(y~job + marital + education + default + housing + loan + contact + day_of_week
                      + campaigngroup + contact_in_last_month + previousgroup
                     + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup
                     + nr.employed + agegroup,
                     data=train, method="rpart",
                     metric = "ROC",
                     trControl = fitControl,
                     tuneLength=5)

cartfit2<-rpart(y~job + marital + education + default + housing + loan + contact + day_of_week 
                + campaigngroup + contact_in_last_month + previousgroup 
                + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup 
                + nr.employed + agegroup,
                data=train,  method="class",control=rpart.control(minsplit=100, cp=0.001))

cartpred<-predict(cartfit, newdata=cv, type="prob")[,"yes"]
cartroc<-roc(cv$y, cartpred)
cartauc<-cartroc$auc

cartpred_test<-predict(cartfit, newdata=test, type="prob")[,"yes"]
cartroc_test<-roc(test$y, cartpred_test)
cartauc_test<-cartroc_test$auc


## Conditional Inference Tree

set.seed(1985)

ctreefit<-train(y~job + marital + education + default + housing + loan + contact + day_of_week
                + campaigngroup + contact_in_last_month + previousgroup
               + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup
               + nr.employed + agegroup,
               data=train, method="ctree",
               metric = "ROC",
               trControl = fitControl,
               tuneLength=5)

ctreepred<-predict(ctreefit, newdata=cv, type="prob")[,"yes"]
ctreeroc<-roc(cv$y, ctreepred)
ctreeauc<-ctreeroc$auc

ctreepred_test<-predict(ctreefit, newdata=test, type="prob")[,"yes"]
ctreeroc_test<-roc(test$y, ctreepred_test)
ctreeauc_test<-ctreeroc_test$auc



## Gradient Boosted Trees
set.seed(1986)

gbmfit<-train(y~job + marital + education + default + housing + loan + contact + day_of_week
               + campaigngroup + contact_in_last_month + previousgroup
               + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup
               + nr.employed + agegroup,
               data=train, method="gbm",
               metric = "ROC",
               trControl = fitControl,
               verbose=TRUE,
               tuneLength=5)

gbmpred<-predict(gbmfit, newdata=cv, type="prob")[,"yes"]
gbmroc<-roc(cv$y, gbmpred)
gbmauc<-gbmroc$auc

gbmpred_test<-predict(gbmfit, newdata=test, type="prob")[,"yes"]
gbmroc_test<-roc(test$y, gbmpred_test)
gbmauc_test<-gbmroc_test$auc

## Random Forest
set.seed(1988)

rffit<-train(y~job + marital + education + default + housing + loan + contact + day_of_week 
              + campaigngroup + contact_in_last_month + previousgroup 
              + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup 
              + nr.employed + agegroup,
              data=train, method="rf", 
              metric = "ROC", 
              trControl = fitControl,
              verbose=FALSE,
              tuneLength=5)
varImpPlot(rffit,type=1)

rfpred<-predict(rffit, newdata=cv, type="prob")[,"yes"]
rfroc<-roc(cv$y, rfpred)
rfauc<-rfroc$auc

rfpred_test<-predict(rffit, newdata=test, type="prob")[,"yes"]
rfroc_test<-roc(test$y, rfpred_test)
rfauc_test<-rfroc_test$auc


## Earth model
set.seed(1983)

marsfit<-train(y~job + marital + education + default + housing + loan + contact + day_of_week
            + campaigngroup + contact_in_last_month + previousgroup
             + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup
             + nr.employed + agegroup,
             data=train, method="earth",
             glm = list(family=binomial),
             metric = "ROC",
             trControl = fitControl,
             tuneLength=5)

marspred<-predict(marsfit, newdata=cv, type="prob")[,"yes"]
marsroc<-roc(cv$y, marspred)
marsauc<-marsroc$auc

marspred_test<-predict(marsfit, newdata=test, type="prob")[,"yes"]
marsroc_test<-roc(test$y, marspred_test)
marsauc_test<-marsroc_test$auc

ensem1<-(gbmpred + rfpred + ctreepred + glmpred)/4
ensem1roc<-roc(cv$y, ensem1)
ensem1auc<-ensem1roc$auc

ensem1_test<-(gbmpred_test + rfpred_test + ctreepred_test + glmpred_test)/4
ensem1roc_test<-roc(test$y, ensem1_test)
ensem1auc_test<-ensem1roc_test$auc

ensem2<-(gbmpred*4+rfpred*3+ctreepred*2+glmpred)/10
ensem2roc<-roc(cv$y, ensem2)
ensem2auc<-ensem2roc$auc

ensem2_test<-(gbmpred_test*4+rfpred_test*3+ctreepred_test*2+glmpred_test)/10
ensem2roc_test<-roc(test$y, ensem2_test)
ensem2auc_test<-ensem2roc_test$auc


auctable<-data.frame(model=c("glm","glmboost","cart","ctree","gbm","rf","mars","Ens 1", "Ens 2"),
                     auc=c(glmauc,glmBoostauc,cartauc, ctreeauc, gbmauc, rfauc, marsauc, ensem1auc, ensem2auc))
auctable<-auctable[order(auctable$auc,decreasing=TRUE),]


auctable_test<-data.frame(model=c("glm","glmboost","cart","ctree","gbm","rf","mars","Ens 1", "Ens 2"),
                     auc=c(glmauc_test,glmBoostauc_test,cartauc_test, ctreeauc_test, gbmauc_test, rfauc_test, marsauc_test, ensem1auc_test, ensem2auc_test))
auctable_test<-auctable_test[order(auctable_test$auc,decreasing=TRUE),]


rffit2<-randomForest(y~job + marital + education + default + housing + loan + contact + day_of_week 
                     +                      + campaigngroup + contact_in_last_month + previousgroup 
                     +                      + poutcome + emp.var.rate + conspricegroup + consconfgroup + euriborgroup 
                     +                      + nr.employed + agegroup,
                     +                      data=train, ntree = 500, importance = TRUE)
varImpPlot(rffit2)[1:18,1]
Importance<-Importance[order(-Importance$MeanDecreaseAccuracy),]
ggplot(Importance, aes(y=Importance$MeanDecreaseAccuracy, 
                       x=Importance$Factors,
                       fill=-Importance$MeanDecreaseAccuracy))+
                      geom_bar(stat="identity")+coord_flip()+
                      ylab("Mean Decrease in Accuracy")+
                      xlab("Variables")+theme(legend.position="none")+
                      ggtitle("Importance of Variable Measured by Mean Decrease in Accuracy")

