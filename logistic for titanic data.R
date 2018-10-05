setwd("D:")#set directiry
set.seed(123)#set seed
library(caret)
library(dplyr)
library(magrittr)
install.packages("e1071")
library(e1071)
a<-read.csv("titanic_train.csv")#read the data using read.csv() and assign it to a dataframe.
View(a)

colSums(is.na(a))#check for null values
p<-a[,-c(1,8,3,9,10,12,13,11,7,14)]#select the null value columns and make it a vector and omit them.
p<-na.omit(p)
View(p)

r<-createDataPartition(p$survived,p=0.9,list=FALSE)#create a datapartition and select your Y,and give to split for train and test, and list as false
train<-p[r,]#your training split 
test<-p[-r,]#your test split
model1<-glm(survived~.,train,family = binomial)#build your model(glm(Y~.,train))and assign it to a dataframe
summary(model1)            
f<-predict(model1,test,"response")#use predict to predict using our model and test dataset
summary(f)
class<-ifelse(f<0.6,0,1)#give the threshold for 0s and 1s
confusionMatrix(factor(test$survived),factor(class))#build the confusion matrix to check for accuracy and TP,TN,FP,FN,Rsquare adjusted R square etc
View(f)
