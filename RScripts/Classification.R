rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data")
getwd()
require(class)
library(caret)
library(klaR)
library("rpart")
library("rpart.plot")
library(randomForest)

#Data Loading
hrData <- read.delim("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data/HR_comma_sep.csv", sep = ",", header= TRUE)
pairs(hrData)
attach(hrData)
leaveSat = hrData[,1]
leaveSat = ifelse(leaveSat < 0.7,1,0)
satPred <- mean(leaveSat != hrData$left)
satPred
hist(hrData$satisfaction_level)
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

#Error calculation
LG_train_err <- sum(abs(hrLGPred_train- leftTrue_train))/length(leftTrue_train) #0.0870
LG_test_err <- sum(abs(hrLGPred_test- leftTrue_test))/length(leftTrue_test) #0.1098

modelName = c(modelName,'Logistic regression')
testErrVector = c(testErrVector,LG_test_err)
trainErrVector = c(trainErrVector,LG_train_err)

##############################################
#LDA
##############################################
hrLDAMod <- lda(left~., data = hr_train)
summary(hrLDAMod)

#Predicting
hrLDAPred_test <- predict(hrLDAMod, newdata = hr_test, type = "response")
hrLDAPred_train <- predict(hrLDAMod, newdata = hr_train, type = "response")

LDA_test_err <- mean(hrLDAPred_test$class != hr_test$left)
LDA_train_err <- mean(hrLDAPred_train$class != hr_train$left)

modelName = c(modelName,'LDA')
testErrVector = c(testErrVector,LDA_test_err)
trainErrVector = c(trainErrVector,LDA_train_err)


##############################################
#QDA
##############################################
hrQDAMod <- qda(left~., data = hr_train)
summary(hrQDAMod)

#Predicting
hrQDAPred_test <- predict(hrQDAMod, newdata = hr_test, type = "response")
hrQDAPred_train <- predict(hrQDAMod, newdata = hr_train, type = "response")

QDA_test_err <- mean(hrQDAPred_test$class != hr_test$left)
QDA_train_err <- mean(hrQDAPred_train$class != hr_train$left)

modelName = c(modelName,'QDA')
testErrVector = c(testErrVector,QDA_test_err)
trainErrVector = c(trainErrVector,QDA_train_err)

#####################################
#TREES
#####################################
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
hrTreeMod <- rpart(left~., data = hr_train, method = "class", control = model.control)
plot(hrTreeMod, uniform = T, compress = T)
text(hrTreeMod, cex = 0)
min_cp = which.min(hrTreeMod$cptable[,4])


hrPruneTreeMod <- prune(hrTreeMod, cp = hrTreeMod$cptable[min_cp,1])

## plot the full tree and the pruned tree
#rpart.plot(hrPruneTreeMod, compress=T,uniform = T)
#text(hrPruneTreeMod, cex = 0.5,pretty = T)
prp(hrPruneTreeMod, , fallen.leaves = FALSE, type=4, extra=1, varlen=0, faclen=0, yesno.yshift=-1)


hrPruneTreePred_test <- predict(hrPruneTreeMod, newdata = hr_test,type = 'class')
hrPruneTreePred_train <- predict(hrPruneTreeMod, newdata = hr_train, type='class')
hrPruneTreePred_valid <- predict(hrPruneTreeMod, newdata = hr_valid, type='class')

PruneTree_test_err <- mean(hrPruneTreePred_test != hr_test$left)
PruneTree_train_err <- mean(hrPruneTreePred_train != hr_train$left)
PruneTree_valid_err <- mean(hrPruneTreePred_valid != hr_valid$left)

modelName = c(modelName,'PruneTree')
testErrVector = c(testErrVector,PruneTree_test_err)
trainErrVector = c(trainErrVector,PruneTree_train_err)


#############################################
#Random Forest
#############################################

hrRFMod = randomForest(left~.,data = hr_train, n.tree =10000)
varImpPlot(hrRFMod)

hrRFPred_test <- predict(hrRFMod, newdata = hr_test,type='response')
hrRFPred_train <- predict(hrRFMod, newdata = hr_train, type='class')
hrRFPred_valid <- predict(hrRFMod, newdata = hr_valid, type='class')

RF_test_err <- mean(hrRFPred_test != hr_test$left)
RF_train_err <- mean(hrRFPred_train != hr_train$left)
RF_valid_err <- mean(hrRFPred_valid != hr_valid$left)

modelName = c(modelName,'RF')
testErrVector = c(testErrVector,RF_test_err)
trainErrVector = c(trainErrVector,RF_train_err)
