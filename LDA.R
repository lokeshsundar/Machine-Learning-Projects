setwd("C:\\Users\\faculty\\Google Drive\\From March 21\\GL - Projects\\Project 4\\working")
getwd()
electiondata<-read.csv(file.choose(), header=T)
View(electiondata) 
attach(electiondata)
str(electiondata)



# Understanding the Pattern through Boxplots 

boxplot(Educationcode~result, col=c("Red","Green"),main="Pattern of Winners by Education", ylab ="Education level", data =electiondata) 

boxplot(agegroup~result, col=c("Blue","Orange"), main="Pattern of Winners by Agegroup", ylab ="Agegroup")

boxplot(Category~result, col=c("Blue","Orange"), main="Pattern of Winners by Category", ylab ="Caste Category")

boxplot(CasesinRange~result,col=c("Purple","Pink"), main="Pattern Revealed By Cases", ylab ="Cases in Range")


electiondata$result=factor(electiondata$result)
str(electiondata)

data<-electiondata
View(data)
prop.table((table(data$result))) 


library(ggplot2) 
ggplot(data, aes(x=result, y=POLL.PERCENTAGE, fill=result)) + geom_boxplot() 

ggplot(data, aes(x=result, y=Educationcode, fill=result)) + geom_boxplot() 

ggplot(data, aes(x=result, y=agegroup, fill=result)) + geom_boxplot() 

ggplot(data, aes(x=result, y=PAN.given, fill=result)) + geom_boxplot() 

ggplot(data, aes(x=result, y=No.of.Candiadates, fill=result)) + geom_boxplot() 

ggplot(data, aes(x=result, y=TOTAL.VOTES.POLLED, fill=result)) + geom_boxplot()

ggplot(data, aes(x=result, y=TOTALVALIDVOTESPOLLED, fill=result)) + geom_boxplot() 


## Scale X variables
x <- subset(data,select=c(12:27,29:34))
scaled_x=scale(x) 
data1=cbind(data[28],scaled_x) 
## Split into training and test sets 
library(caTools) 
set.seed(123) 
split = sample.split(data1$result, SplitRatio = 0.7) 
traindata = subset(data1, split == TRUE) 
testdata = subset(data1, split == FALSE) 

#### Check if distribution of partition data is correct Testing dataset
prop.table((table(traindata$result))) 
prop.table((table(testdata$result)))

View(traindata)
View(testdata)

X1=cbind(as.matrix(traindata[,2:23]))
Y1=as.vector(traindata[,1]) 
Manova=manova(X1~Y1) 
summary(Manova)
summary(Manova,test = "Wilks") 
View(X1)
write.csv(X1,"x1.csv")
View(Y1)
is.na(X1)


library(DiscriMiner)
library(MASS) 
discPower(X1,Y1)
View(X1)
View(Y1)
desDA(X1,Y1) 



library(corrplot)
correlations<- cor(X1)
corrplot(correlations, method="circle")

sublda=lda(result~.,data = traindata)
sublda 

write.csv(sublda$means, "diff_in_group_means.csv") 

plot(sublda, dimen = 1, type = "b")

LD <- data.frame(predict(sublda)$x, class = Y1) 


# Density plot

library(ggplot2)
p1 <- ggplot(LD, aes(x = LD1)) +  
geom_density(aes(fill = as.factor(class)), alpha=0.7, position="identity") + 
theme_bw() +  
theme(legend.position = "none") +  
ylab("Density")
p1 


# Prediction
  lda.pred=predict(sublda, newdata = testdata) 
library(hmeasure) 
class.lda=lda.pred$class
true.class<-testdata[,1] 
lda.counts <- misclassCounts(class.lda,true.class) 
lda.counts$conf.matrix 
  
#Evaluation
  
print(lda.counts$metrics,digits=3) 


library(ROCR)
library(ineq)

testdata$predict.score <- predict(sublda,testdata, type="response")
View(testdata)

pred <- prediction(testdata$predict.score[3],testdata$result)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = ineq(as.numeric(unlist(testdata$predict.score), type="Gini"))
auc
KS
gini























