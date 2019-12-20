newdata<-read.csv('train.csv')
newdata_1<-read.csv('test.csv')
submission<-read.csv('gender_submission.csv')
head(newdata)
#head(submission)

#clean the dataset 

attach(newdata)
data<-data.frame(PassengerId,Survived,Pclass,Sex,Age,SibSp,Parch,Fare,Embarked)
detach(newdata)


data$Age[is.na(data$Age)]<-median(data$Age,na.rm = TRUE)
head(data)
data$Sex<-as.factor(data$Sex)
data$Embarked<-as.factor(data$Embarked)
data$Embarked[data$Embarked==""]<-"S"
data$Embarked<- factor(c('C','Q','S'))
levels(data$Embarked)
#data$Survived<-as.factor(data$Survived)
head(data)
data.1<-data[,-1:-2]
options(max.print=10000) 
data.1

head(data.1)

#sapply(data.1,class)
#install.packages('randomForest')
library(randomForest)
fit.forest <-randomForest(data.1,as.factor(data$Survived),importance=TRUE)

fit.forest

#summary(fit.forest)
#importance(fit.forest, type=2)
#newdata_1
attach(newdata_1)
data_2<-data.frame(Pclass,Sex,Age,SibSp,Parch,Fare,Embarked)
detach(newdata_1)

data_2$Sex<-as.factor(data_2$Sex)
data_2$Embarked<-as.factor(data_2$Embarked)
data_2$Age[is.na(data_2$Age)]<-median(data_2$Age,na.rm = TRUE)
data_2$Fare[is.na(data_2$Fare)]<-mean(data_2$Fare,na.rm = TRUE)
head(data.1)
head(data_2)
sapply(data.1,class)
sapply(data_2,class)


forest.pred <- predict(fit.forest,data_2)


submission<-data.frame(newdata_1$PassengerId)
names(submission)[1]<-"PassengerId"
newdata_1$PassengerId
submission$Survived<-forest.pred
write.csv(submission, file = "1_random_forest_r_submission.csv",row.names = FALSE)




















