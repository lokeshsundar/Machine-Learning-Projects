#Some libraries

library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
library(ggord)
library(ggplot2)
library(Hmisc)
library(klaR)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
library(SDMTools)
library(dplyr)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
install.packages('caret', dependencies=TRUE)
setwd("C:\\Users\\faculty\\Google Drive\\From March 21\\GL - Projects\\Project 4\\working")
getwd()
electiondataoverall<-read.csv(file.choose(), header=T)
summary(electiondataoverall)
str(electiondataoverall)


#Correlation for Election Data

View(electiondataoverall)

electiondataforcorr<-subset(electiondataoverall[,c(12:34)])
str(electiondataforcorr)
cor(electiondataforcorr)
#install.packages("corrplot")


library(corrplot)
correlations<- cor(electiondataforcorr)
corrplot(correlations, method="circle")

####variable SelectionSex


#Variables...does borrowing purpose explain
boxplot(electiondataoverall$Educationcode  ~electiondataoverall$result)
aov.educationcode<-aov(electiondataoverall$result  ~as.factor(electiondataoverall$Educationcode))
summary(aov.educationcode)
tk.category<-TukeyHSD(aov.educationcode)
tk.category

boxplot(electiondataoverall$agegroup  ~electiondataoverall$result)
aov.agegroup<-aov(electiondataoverall$result  ~as.factor(electiondataoverall$agegroup))
summary(aov.agegroup)
tk.category<-TukeyHSD(aov.agegroup)
tk.category



#######Performing Regression

#Partitioning Data Sets
#Partition train and val
#We will use this throughout so that samples are comparable
set.seed(1234)
pd<-sample(2,nrow(electiondataoverall),replace=TRUE, prob=c(0.7,0.3))

pd1<-sample(2,nrow(electiondataoverall),replace=TRUE, prob=c(0.7,0.3))

train<-electiondataoverall[pd==1,]
val<-electiondataoverall[pd==2,]


sum(electiondataoverall$result)
sum(val$result)
sum(train$result)


#Data Frame for Linear Regression

train.reg<-train[,c(12:34)]
val.reg<-val[,c(12:34)]

str(train.reg)
View(train.reg)



###Now some basic commands
linearmodel1<-lm(train.reg$TOTALVALIDVOTESPOLLED  ~ Category, data=train.reg)
summary(linearmodel1)

linearmodel2<-lm(train.reg$TOTAL.VOTES.POLLED  ~ POLL.PERCENTAGE, data=train.reg)
summary(linearmodel2)

linearmodel3<-lm(train.reg$TOTALVALIDVOTESPOLLED  ~Incumbent.contestant, data=train.reg)
summary(linearmodel3)

linearmodel3<-lm(train.reg$TOTALVALIDVOTESPOLLED  ~Incumbent.party, data=train.reg)
summary(linearmodel3)

linearmodel5<-lm(train.reg$TOTALVALIDVOTESPOLLED  ~., data=train.reg)
summary(linearmodel5)


# Fit regression line
View(train.reg)
train.plot<-train.reg[,c(23,5)]
val.plot<-val.reg[,c(23,5)]
OLS.plot<-lm(linearmodel1, data=train.plot)
summary(OLS.plot)
par(mgp=c(2,1,0), mar=c(3,3,1,1))

require(stats)
coeff=coefficients(OLS.plot)
coeff
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1),  "*x ", round(coeff[1],1))
# plot
plot(train.plot, main=eq)
abline(OLS.plot, col="blue")
plot(val.plot, main=eq)
abline(OLS.plot, col="red")


###Multiple Regression with full Model

OLS.full<-lm(TOTALVALIDVOTESPOLLED ~ ., data=train.reg)
summary(OLS.full)

#Drop insignificant ones
Linear.final<-TOTALVALIDVOTESPOLLED  ~agegroup+Sex+CasesinRange+Filled.ITR+DMK.Participant+ADMK.Participant+No.of.Candiadates+result+TOTAL.VOTES.POLLED+TotalElectors+POLL.PERCENTAGE 
OLS.final<-lm(Linear.final,data=train.reg)
summary(OLS.final)
vif(OLS.final)

Linear.final<-TOTALVALIDVOTESPOLLED  ~agegroup+Sex+CasesinRange+Filled.ITR+DMK.Participant+ADMK.Participant+No.of.Candiadates+result+TotalElectors+POLL.PERCENTAGE 
OLS.final<-lm(Linear.final,data=train.reg)
summary(OLS.final)
vif(OLS.final)

OLS.finalonval<-lm(Linear.final,data=val.reg)
summary(OLS.finalonval)
vif(OLS.finalonval)
pred.reg<-predict(OLS.final,newdata=val.reg, interval="predict")
head(pred.reg)
tail(pred.reg)
mse1 <- mean((val.reg$TOTALVALIDVOTESPOLLED- pred.reg[1])^2)
print(mse1)
write.csv(pred.reg,"pre.csv")
write.csv(val.reg$TOTALVALIDVOTESPOLLED,"valvote.csv")

###########
#Linear Probability Model
View(train)
train.lpm<-train[,c(12:34)]
val.lpm<-val[,c(12:34)]

LPM.1<-result  ~ Incumbent.party  


OLS.LPM.1<-lm(LPM.1,train.lpm)
summary(OLS.LPM.1)

#######


# Fit regression line
train.plot.LPM<-train[,c(28,30)]
val.plot.LPM<-val[,c(28,30)]
OLS.plot.LPM<-lm(LPM.1, data=train.plot.LPM)
summary(OLS.plot.LPM)
par(mgp=c(2,1,0), mar=c(3,3,1,1))

require(stats)

coeff=coefficients(OLS.plot.LPM)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(train.plot.LPM, main=eq)
abline(OLS.plot.LPM, col="blue")
plot(val.plot.LPM, main=eq)
abline(OLS.plot.LPM, col="red")

## Linear Probability Model using All Variables
OLS.LPM<-result  ~.
OLS.LPM<-lm(LPM,train.lpm)
summary(OLS.LPM)

#after Dropping Insignificant Variables #Retain only the significant ones

LPM.Final<-result  ~Educationcode+Sex+DMK.Participant+Incumbent.party+TOTALVALIDVOTESPOLLED
LPM.Final<-lm(LPM.Final,train.lpm)
summary(LPM.Final)
vif(LPM.Final)


#Now some Predictions
Pred_LPM <- predict(LPM.Final,newdata=val.lpm)
head(Pred_LPM)
Pred_LPM
#Confusionmatrix
tab.LPM<-table(val.lpm$result, Pred_LPM > 0.5)
tab.LPM
sum(diag(tab.LPM))/sum(tab.LPM)

#LPM MOdel Evaluation
val.lpm$predict.score <- predict(LPM.Final,val.lpm, type="response")

##install.packages("ROCR")
##install.packages("ineq")
library(ROCR)
library(ineq)

val.lpm$predict.score <- predict(LPM.Final,val.lpm, type="response")

pred <- prediction(val.lpm$predict.score, val.lpm$result)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(val.lpm$predict.score, type="Gini")
auc
KS
gini




#Logistic Regression

install.packages(c("SDMTools","pROC", "Hmisc"))
library(SDMTools)
library(pROC)
library(Hmisc)

#data frame
View(train)
train.logit<-train[,c(12:34)]
val.logit<-val[,c(12:34)]


# Fit the Sigmoid function
#All Variables

Logit.eq<-result   ~.
Logit <- glm(Logit.eq, train.logit, family = binomial)
summary(Logit)
vif(Logit)


#####Retain only significant ones

Logit.eq.final<-result  ~ PAN.given+TotalElectors+POLL.PERCENTAGE+TOTALVALIDVOTESPOLLED
Logit.Final <- glm(Logit.eq.final, train.logit, family = binomial)
summary(Logit.Final)
vif(Logit.Final)

## MCFadden's R2 

modLogit <- glm(result~PAN.given+TotalElectors+POLL.PERCENTAGE+TOTALVALIDVOTESPOLLED, train.logit, family = binomial)
nullmodligit<-glm(electiondataoverall$result~1,family="binomial")
mcfaddenr2<-1-logLik(modLogit)/logLik(nullmodligit)
mcfaddenr2
###

## Logistic Regression New

## Stepwise Logistic Regression
Logit.Final = step(Logit.Final)

#Logistic Regression Coefficient
summary.coeff0 = summary(Logit.Final)$coefficient
summary.coeff0
###

### Odds Ratio

confint(Logit.Final)

confint.default(Logit.Final)

exp(coef(Logit.Final))

exp(cbind(OR = coef(Logit.Final), confint(Logit.Final)))

logLik(Logit.Final)



pred.logit.final <- predict.glm(Logit.Final, newdata=val.logit, type="response")

head(pred.logit.final)
#Classification

library(caret)

tab.logit<-confusion.matrix(val.logit$result,pred.logit.final,threshold = 0.5)
tab.logit
accuracy.logit<-roc.logit<-roc(val.logit$result,pred.logit.final )
roc.logit
plot(roc.logit)


ROC<- data.frame( Result=val.logit$result, Fitted = pred.logit.final)
ROC
write.csv(ROC, file = "ROC.csv")



##install.packages("ROCR")
##install.packages("ineq")
library(ROCR)
library(ineq)

val.logit$predict.score <- predict(Logit.Final, val.logit, type="response")
pred <- prediction(val.logit$predict.score, val.logit$result)
# perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(val.logit$predict.score, type="Gini")
auc
KS
gini





####How did the two fair?
#LPM
accuracy.LPM<-sum(diag(tab.LPM))/sum(tab.LPM)
accuracy.LPM
loss.LPM<-tab.LPM[2,1]/(tab.LPM[2,1]+tab.LPM[1,1])
loss.LPM
opp.loss.LPM<-tab.LPM[1,2]/(tab.LPM[1,2]+tab.LPM[2,2])
opp.loss.LPM
tot.loss.LPM<-0.95*loss.LPM+0.05*opp.loss.LPM
tot.loss.LPM
#Logit
accuracy.logit<-sum(diag(tab.logit))/sum(tab.logit)
accuracy.logit
loss.logit<-tab.logit[1,2]/(tab.logit[1,2]+tab.logit[1,1])
loss.logit
opp.loss.logit<-tab.logit[2,1]/(tab.logit[2,1]+tab.logit[2,2])
opp.loss.logit
tot.loss.logit<-0.95*loss.logit+0.05*opp.loss.logit
tot.loss.logit












