rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data")
getwd()
require(class)
library(caret)
library(klaR)
library("rpart")
library("rpart.plot")
library(randomForest)
library(gbm)
library(quantmod)
library(e1071)
library("ggplot2")
library("pROC")
library(psych)

#Data Loading
hrData <- read.delim("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data/HR_comma_sep.csv", sep = ",", header= TRUE)
hrData$left <- as.factor(hrData$left)

leaveSat = hrData[,1]
leaveSat = as.factor(ifelse(leaveSat < 0.7,1,0))
satPred <- mean(leaveSat != hrData$left)
satPred

hist(hrData$satisfaction_level)
hrData$satisfaction_level = NULL
#pairs(hrData)
attach(hrData)

set.seed(667)
validIndex <- sample(1:nrow(hrData), .10*nrow(hrData))
hr_valid <- hrData[validIndex,]
hr_pend <- hrData[-validIndex,]

set.seed(665)
trainIndex <- sample(1:nrow(hr_pend), .66*nrow(hr_pend))
hr_train <- hrData[trainIndex,]
hr_test <- hrData[-trainIndex,]


#Numerical response data
#leftTrue_train <- as.numeric(hr_train$left)-1
#leftTrue_test <- as.numeric(hr_test$left)-1

#Error holder
modelName = c()
testErrVector = c()
trainErrVector = c()
##############################################
#Logistic Regression
##############################################

#Model Builiding
hrLGMod <- glm(left ~., data = hr_train, family = "binomial")
summary(hrLGMod)
names(hrLGMod)

#Predicting
hrLGPred_test <- predict.glm(hrLGMod, newdata = hr_test, type = "response")
hrLGPred_train <- predict(hrLGMod, newdata = hr_train, type = "response")



#Rounding
hrLGPred_test = round(hrLGPred_test)
hrLGPred_train = round(hrLGPred_train)
LGRocCurve <- roc(as.numeric(as.factor(hrLGPred_test)),as.numeric(hr_test$left))
#Error calculation
LG_test_err <- mean(as.factor(hrLGPred_test) != hr_test$left)
LG_train_err <- mean(as.factor(hrLGPred_train) != hr_train$left)

modelName = c(modelName,'Logistic regression')
testErrVector = c(testErrVector,LG_test_err)
trainErrVector = c(trainErrVector,LG_train_err)

##############################################
#LDA
###############################################
#hrLDAMod <- lda(left~., data = hr_train)
#summary(hrLDAMod)
#
##Predicting
#hrLDAPred_test <- predict(hrLDAMod, newdata = hr_test, type = "response")
#hrLDAPred_train <- predict(hrLDAMod, newdata = hr_train, type = "response")
#
#LDARocCurve <- roc(as.numeric(hrLDAPred_test$class),as.numeric(hr_test$left))
#
#LDA_test_err <- mean(hrLDAPred_test$class != hr_test$left)
#LDA_train_err <- mean(hrLDAPred_train$class != hr_train$left)
#
#modelName = c(modelName,'LDA')
#testErrVector = c(testErrVector,LDA_test_err)
#trainErrVector = c(trainErrVector,LDA_train_err)


##############################################
#QDA
##############################################
#hrQDAMod <- qda(left~., data = hr_train)
#summary(hrQDAMod)
#
##Predicting
#hrQDAPred_test <- predict(hrQDAMod, newdata = hr_test, type = "response")
#hrQDAPred_train <- predict(hrQDAMod, newdata = hr_train, type = "response")
#
#QDARocCurve <- roc(as.numeric(hrQDAPred_test$class),as.numeric(hr_test$left))
#
#QDA_test_err <- mean(hrQDAPred_test$class != hr_test$left)
#QDA_train_err <- mean(hrQDAPred_train$class != hr_train$left)
#
#
#
#modelName = c(modelName,'QDA')
#testErrVector = c(testErrVector,QDA_test_err)
#trainErrVector = c(trainErrVector,QDA_train_err)

#####################################
#TREES
#####################################
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
hrTreeMod <- rpart(left~., data = hr_train, method = "class", control = model.control)
plot(hrTreeMod, uniform = T, compress = T)
text(hrTreeMod, cex = 0.5)
min_cp = which.min(hrTreeMod$cptable[,4])


hrPruneTreeMod <- prune(hrTreeMod, cp = hrTreeMod$cptable[min_cp,1])

## plot the full tree and the pruned tree
#rpart.plot(hrPruneTreeMod, compress=T,uniform = T)
#text(hrPruneTreeMod, cex = 0.5,pretty = T)
#prp(hrPruneTreeMod, , fallen.leaves = FALSE, type=4, extra=1, varlen=0, faclen=0, yesno.yshift=-1)


hrPruneTreePred_test <- predict(hrPruneTreeMod, newdata = hr_test,type = 'class')
hrPruneTreePred_train <- predict(hrPruneTreeMod, newdata = hr_train, type='class')
hrPruneTreePred_valid <- predict(hrPruneTreeMod, newdata = hr_valid, type='class')

PruneTreeRocCurve <- roc(as.numeric(hrPruneTreePred_test),as.numeric(hr_test$left))

PruneTree_test_err <- mean(hrPruneTreePred_test != hr_test$left)
PruneTree_train_err <- mean(hrPruneTreePred_train != hr_train$left)
PruneTree_valid_err <- mean(hrPruneTreePred_valid != hr_valid$left)

modelName = c(modelName,'Single Pruned Tree')
testErrVector = c(testErrVector,PruneTree_test_err)
trainErrVector = c(trainErrVector,PruneTree_train_err)


#############################################
#Random Forest
#############################################

hrRFMod = randomForest(left~.,data = hr_train, n.tree =10000)
#varImpPlot(hrRFMod)

hrRFPred_test <- predict(hrRFMod, newdata = hr_test,type='class')
hrRFPred_train <- predict(hrRFMod, newdata = hr_train, type='class')
hrRFPred_valid <- predict(hrRFMod, newdata = hr_valid, type='class')

RFRocCurve <- roc(as.numeric(hrRFPred_test),as.numeric(hr_test$left))

RF_test_err <- mean(hrRFPred_test != hr_test$left)
RF_train_err <- mean(hrRFPred_train != hr_train$left)
RF_valid_err <- mean(hrRFPred_valid != hr_valid$left)

modelName = c(modelName,'Random Forest')
testErrVector = c(testErrVector,RF_test_err)
trainErrVector = c(trainErrVector,RF_train_err)

#############################################
#Bagging
############################################
hrBAGMod = randomForest(left~.,data = hr_train, n.tree =10000, mtry = 8)
#varImpPlot(hrBAGMod)

hrBAGPred_test <- predict(hrBAGMod, newdata = hr_test,type='class')
hrBAGPred_train <- predict(hrBAGMod, newdata = hr_train, type='class')
hrBAGPred_valid <- predict(hrBAGMod, newdata = hr_valid, type='class')


BAGRocCurve <- roc(as.numeric(hrBAGPred_test),as.numeric(hr_test$left))


BAG_test_err <- mean(hrBAGPred_test != hr_test$left)
BAG_train_err <- mean(hrBAGPred_train != hr_train$left)
BAG_valid_err <- mean(hrBAGPred_valid != hr_valid$left)

modelName = c(modelName,'Bagging')
testErrVector = c(testErrVector,BAG_test_err)
trainErrVector = c(trainErrVector,BAG_train_err)

############################################
#Boosting
############################################
#dep = floor(sqrt(NCOL(data)))
#boost_train = hr_train
#boost_train$left = as.numeric(boost_train$left)-1
#hrBOOSTMod = gbm(left~.,data = boost_train, n.tree =100,shrinkage = .0001 ,interaction.depth = dep,distribution = 'adaboost')
#
#hrBOOSTPred_test <- predict(hrBOOSTMod, newdata = hr_test,type='response', n.trees = 100)
#hrBOOSTPred_train <- predict(hrBOOSTMod, newdata = hr_train,type='response', n.trees = 1000)
#hrBOOSTPred_valid <- predict(hrBOOSTMod, newdata = hr_valid,type='response', n.trees = 1000)
#
#BOOST_test_err <- mean(hrBOOSTPred_test != hr_test$left)
#BOOST_test_err
#BOOST_train_err <- mean(hrBOOSTPred_train != hr_train$left)
#BOOST_valid_err <- mean(hrBOOSTPred_valid != hr_valid$left)
#
#modelName = c(modelName,'Boosting')
#testErrVector = c(testErrVector,BOOST_test_err)
#trainErrVector = c(trainErrVector,BOOST_train_err)

errorDF = data.frame(Model_Name = modelName,Training_Error = trainErrVector,Test_Error = testErrVector)

ggplot(errorDF, aes(x = Model_Name,group = 1)) + 
  geom_line(aes(y = testErrVector), colour="#3EBCC0") + 
  geom_line(aes(y = trainErrVector), colour = "#F88179") +
  scale_x_discrete(limits=c("Logistic regression","LDA","QDA","Single Pruned Tree","Random Forest","Bagging"))+
  geom_point(aes(y = testErrVector), colour="#3EBCC0") + 
  geom_point(aes(y = trainErrVector), colour = "#F88179") +
  labs(y = "Error",x= "Models") +
  scale_colour_manual(values=c("red", "blue"))
  

errorDFMelt = melt(errorDF[,c("Model_Name","Training_Error","Test_Error")])
ggplot(errorDFMelt, aes(x = Model_Name, y = value, colour = variable,group = 1)) + geom_line()



ggplot()+
  geom_line(aes(RFRocCurve))

plot(LGRocCurve,col = "Yellow",label = "Logistic Regression")
plot(LDARocCurve, add=TRUE, col='Green',label = "LDA")
plot(QDARocCurve, add=TRUE, col='Pink',label = "QDA")
plot(PruneTreeRocCurve, add=TRUE, col='Orange',label = "PrunedTree")
plot(RFRocCurve, add=TRUE, col='Blue',label = "Random Forest")
plot(BAGRocCurve, add=TRUE, col='red',label = "Bagging")
legend("bottomright",legend = c("Logistic Regression","LDA","QDA","Pruned Tree","Random Forest","Bagging"))



plot(hrData$number_project,hrData$average_montly_hours,col=hrData$left)
pairs(hrData[,c(1:5,7)],col=hrData$left)

hrData[1,c(1:5,7)]


panels(hrData)
pairs.panels(hrData)


draw_confusion_matrix <- function(cm,titleName) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(titleName, cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Stayed', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Left', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Stayed', cex=1.2, srt=90)
  text(140, 335, 'Left', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

confTest = confusionMatrix(hr_test$left,hrBAGPred_test)
confValid = confusionMatrix(hr_valid$left,hrBAGPred_valid)
draw_confusion_matrix(confTest,"Test Data")
draw_confusion_matrix(confValid,"Validation Data")