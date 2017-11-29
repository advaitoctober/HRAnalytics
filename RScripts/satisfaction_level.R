
#read data
data <- read.csv('/Users/advait/Downloads/HR_comma_sep.csv')

#prepare data
new <- data

new$low_salary <- ifelse(data$salary == "low",1,0)
new$med_salary <- ifelse(data$salary == "medium",1,0)
new$high_salary <- ifelse(data$salary == "high",1,0)

depts <- c(levels(new$sales))
for(i in seq(1,10,1))
{
  x <- depts[i]
  m <- ifelse(data$sales == x,1,0)
  new <- cbind(new,m)
}


for(i in seq(1,10,1))
{
  colnames(new)[i+13] <- depts[i]
}
new <- new[,-10]
new <- new[,-9]

data <- new

#test, train and validation

set.seed(667)
validIndex <- sample(1:nrow(data), .10*nrow(data))
hr_valid <- data[validIndex,]
hr_pend <- data[-validIndex,]

set.seed(665)
trainIndex <- sample(1:nrow(hr_pend), .66*nrow(hr_pend))
hr_train <- data[trainIndex,]
hr_test <- data[-trainIndex,]

left_train <- hr_train$left
sat_train <- hr_train$satisfaction_level
drops <- c("left")
hr_train <- hr_train[,!(names(hr_train) %in% drops)]

left_test <- hr_test$left
sat_test <- hr_test$satisfaction_level
drops <- c("left")
hr_test <- hr_test[,!(names(hr_test) %in% drops)]

#median value
median.train <- median(hr_train$satisfaction_level)
#median.left.score <- ifelse(hr_train$satisfaction_level >= 0.65,0,1)

#linear

linear.model <- lm(formula = satisfaction_level~.,data = hr_train)
linear.predictions <- predict(linear.model,newdata = hr_test)
#median.predicted.score <- ifelse(linear.predictions >= 0.65,0,1)

linear.error <- mean((linear.predictions-sat_test)^2)

#1 - mean(median.predicted.score != median.left.score)

#linear using bootstrap

#linear.results <- ((sat_test-linear.predictions)^2)
#library("boot")

#linear.function <- function(formula,data,test_data,indices){
 # d <- data[indices,]
  #fit <- lm(formula,data = d)
  #preds <- predict(fit,data = test_data)
  #return(preds)
#}

#lin.result <- boot(data = hr_train,test_data = hr_test,statistic = linear.function, R =1000, formula = satisfaction_level~.)

#predicted.values <- lin.result$t0
#median.predicted.values <- ifelse(predicted.values >= 0.65,0,1)

#1 - mean(median.predicted.values != median.left.score)

#ridge 
library("glmnet")
x = as.matrix(hr_train)
y = as.numeric(sat_train)
cv.glmnet(x,y,alpha = 0)
ridge.mod = glmnet(x,y,alpha = 0,lambda = 0.02703303)
ridge.predictions <- predict(ridge.mod,newx = as.matrix(hr_test),s =0.02703303)
ridge.error <- mean((ridge.predictions-sat_test)^2)


#lasso

cv.glmnet(x,y,alpha = 1)
lasso.mod = glmnet(x,y,alpha = 1,lambda = 0.007880272)
lasso.predictions <- predict(ridge.mod,newx = as.matrix(hr_test),s = 0.007880272)
lasso.error <- mean((lasso.predictions-sat_test)^2)


#PCA
library("pls")

pcr.fit <- pcr(satisfaction_level~., data = as.data.frame(hr_train), scale = TRUE, validation = "CV")
test_pred <- predict(pcr.fit, newdata = hr_test)
pca.error <- mean((sat_test - test_pred)^2)

#PLS

pls.fit <- plsr(satisfaction_level~., data = as.data.frame(hr_train), scale = TRUE, validation = "CV")
test_pred <- predict(pls.fit,newdata = hr_test)
pls.error <- mean((sat_test - test_pred)^2)


#forward subset
library("leaps")
bests = regsubsets(satisfaction_level ~.,data = hr_train, method = "forward")
summary(bests)
selects <- c("last_evaluation","number_project","time_spend_company","Work_accident","low_salary","promotion_last_5years",
             "high_salary","accounting","satisfaction_level")
temp_train <- hr_train[,(names(hr_train) %in% selects)]
temp_test <- hr_test[,names(hr_test) %in% selects]

linear.modelf <- lm(formula = satisfaction_level~.,data = temp_train)
linear.predictionsf <- predict(linear.modelf,newdata = temp_test)

forward.error <- mean((sat_test-linear.predictionsf)^2)

#backward subset
bests = regsubsets(satisfaction_level ~.,data = hr_train, method = "backward")
summary(bests)
selects <- c("last_evaluation","number_project","time_spend_company","Work_accident","low_salary","promotion_last_5years",
             "average_montly_hours","accounting ","satisfaction_level","med_salary")

temp_train <- hr_train[,(names(hr_train) %in% selects)]
temp_test <- hr_test[,names(hr_test) %in% selects]

linear.modelb <- lm(formula = satisfaction_level~.,data = temp_train)
linear.predictionsb <- predict(linear.modelb,newdata = temp_test)

backeard.error <- mean((sat_test-linear.predictionsb)^2)

#best subset

bests = regsubsets(satisfaction_level ~.,data = hr_train, method = "best")
summary(bests)
selects <- c("last_evaluation,number_project","time_spend_company","Work_accident","low_salary","promotion_last_5years",
             "average_montly_hours","accounting ","satisfaction_level","med_salary")

temp_train <- hr_train[,(names(hr_train) %in% selects)]
temp_test <- hr_test[,names(hr_test) %in% selects]

linear.modelbs <- lm(formula = satisfaction_level~.,data = temp_train)
linear.predictionsbs <- predict(linear.modelbs,newdata = temp_test)

best.error <- mean((sat_test-linear.predictionsb)^2)




#plot errors
error_list <- c(linear.error,ridge.error,lasso.error,pca.error,pls.error,forward.error,backeard.error,best.error)
errors <-  c("linear.error","ridge.error","lasso.error","pca.error","pls.error","forward.error","backeard.error","best.error")


barplot(error_list,xaxt="n",ylab = "Errors",xlab="Methods",col = "blue")
axis(1, at=1:8,labels = errors[1:8])



