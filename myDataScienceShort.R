#load data
rm(list=ls())
A = read.csv("challenge.csv", header=T)

#understand data
summary(A)
sapply(A, function(x) sum(is.na(x)))
sum(complete.cases(A))

#step 1: get the performance of the existing data
#Use Boruta to find important and unimportant attributes, and ignore na cases
#install.packages("Boruta")
library(Boruta)
compData<-A[complete.cases(A),]
b<-Boruta(x=compData[,2:ncol(A)],y=compData[,1],maxRuns=200)
plot(b)
print(b)
impVar<-getSelectedAttributes(b,withTentative=F)
impVar
#use important attribute and random forest to classifiy
#install.packages("randomForest")
library(randomForest)
newData<-A[complete.cases(A[,impVar]),]
fm<-as.formula(paste("response",paste(impVar,collapse="+"),sep="~"))
rf<-randomForest(fm,newData,ntree=500)
print(rf)

#step 2: import the test data before performance analysis
#install.packages("mice")
library(mice)
input<-c(1115, NA, 748, 182, NA, NA, 178, 311, 756, 226, NA, NA, NA, 48, 1009, NA, 204, 593)
B<-rbind(A[,2:ncol(A)],input)
tempData <- mice(B,m=5,maxit=50,meth='pmm',seed=500)
completedData <- complete(tempData,1)
A[,2:ncol(A)]<-completedData[1:(nrow(completedData)-1), ]

#Use Boruta to find important and unimportant attributes
#install.packages("Boruta")
#library(Boruta)
compData<-A[complete.cases(A),]
b<-Boruta(x=compData[,2:ncol(A)],y=compData[,1],maxRuns=200)
plot(b)
print(b)
impVar<-getSelectedAttributes(b,withTentative=F)
impVar

#use important attribute and random forest to classifiy
#install.packages("randomForest")
#library(randomForest)
newData<-A[complete.cases(A[,impVar]),]
fm<-as.formula(paste("response",paste(impVar,collapse="+"),sep="~"))
rf<-randomForest(fm,newData,ntree=500)
print(rf)

#to predict result
idx<-which(names(A)%in%impVar)
inputData<-input[idx]
predData<-completedData[nrow(completedData),impVar,drop=F]
result<-predict(rf,newdata=predData)
result