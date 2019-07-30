
machine=read.csv("Machine_data.csv",na.strings = "-1")
str(machine)
sum(is.na(machine))
machine$id=NULL

str(machine)
names=c(1:18,21,22,23) 
machine[,names]=lapply(machine[,names],factor)

machine=na.omit(machine)

str(machine)


machine$Breakdown=as.factor(machine$Breakdown)


plot(machine$V19,machine$Breakdown)


library(e1071)
library(caret)
trainrows=createDataPartition(machine$Breakdown,p=0.80,list = FALSE)
traind = machine[trainrows, ]
testd = machine[-trainrows, ]
test=length(which(machine$Breakdown==1))/nrow(testd)

length(unique(traind$Breakdown))
length(unique(testd$Breakdown))

train=length(which(machine$Breakdown==1))/nrow(traind)
train

par(mfrow=c(1,2))
plot(machine$V19,machine$Breakdown)
plot(machine$V20,machine$Breakdown)
plot(machine$V19,machine$V20)

library(caret)


#decision tree c5.0

library(C50)
library(caret)
model1 = C5.0(Breakdown~.,data=traind)
summary(model1) 
pred_Train = predict(model1,newdata=traind, type="class")
pred_Test = predict(model1, newdata=testd, type="class") 
confusionMatrix(traind$Breakdown,pred_Train) 
confusionMatrix(testd$Breakdown,pred_Test) 

'----------------------------------------------------------------------------------'
#random forest
library(randomForest)
set.seed(007)
#model3=randomForest(Breakdown~.,data=traind,ntree=150,mtry=8)
plot(model3)
#tree size we got 150
library(caret) 
p1=predict(model3,traind)
confusionMatrix(p1,traind$Breakdown)
p2=predict(model3,testd)
confusionMatrix(p2,testd$Breakdown)
#tune mtry
str(machine)
library(e1071)
#t = tuneRF(traind[,-23],traind[,23],stepFactor = 0.5,ntree=140,mtry =8,trace=TRUE,improve = 0.05)
#mtry value is 2

#no of nodes of the tree
hist(treesize(model3),main="No of nodes for the tree",col = "red")

importance(model3)

'---------------------------------------------------------------------'
library(ada) 
model2 = ada(Breakdown ~ ., iter = 20,data = traind, loss="logistic")
model2
pred = predict(model2, testd)
pred
pred1 = predict(model2, traind)
confusionMatrix(testd$Breakdown,pred,positive = "1")
confusionMatrix(traind$Breakdown,pred1,positive = "1")


'--------------------------------------------------------------'
library(rpart)
model_dt <- rpart(Breakdown ~ . , traind)
# Prediction on the train data
preds_train_dt <- predict(model_dt,traind)
preds_train_tree <- ifelse(preds_train_dt[, 1] > preds_train_dt[, 2], 0, 1)
# Prediction on the test data
preds_dt <- predict(model_dt,testd)
preds_tree <- ifelse(preds_dt[, 1] > preds_dt[, 2], 0, 1)

confusionMatrix(testd$Breakdown,pred,positive = "1")
confusionMatrix(traind$Breakdown,pred1,positive = "1")

'-------------------------------------------------------------------------'
par(mfrow = c(1,2))

#  SVM 
# 
library(e1071)
dummies <- dummyVars(Breakdown~.,data=machine)

x.train=predict(dummies, newdata = traind)
y.train=traind$Breakdown
x.test = predict(dummies, newdata = testd)
y.test = testd$Breakdown
#
# Building the model on train data
model  =  svm(x = x.train, y = y.train, type = "C-classification", kernel = "linear", cost = 0.001)
summary(model)
plot(model)
#
pred_train<-predict(model,x.train)
pred_test<-predict(model,x.test)
# Build Confusion matrix
confusionMatrix(pred_train,y.train)
confusionMatrix(pred_test,y.test)


     