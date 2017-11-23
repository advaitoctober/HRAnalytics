
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
new <- new[,-8]
new <- new[,-8]

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
linear.predictions <- predict(linear.model,data = hr_test)
#median.predicted.score <- ifelse(linear.predictions >= 0.65,0,1)

#1 - mean(median.predicted.score != median.left.score)

#linear using bootstrap

library("boot")

linear.function <- function(formula,data,test_data,indices){
  d <- data[indices,]
  fit <- lm(formula,data = d)
  preds <- predict(fit,data = test_data)
  return(preds)
}

lin.result <- boot(data = hr_train,test_data = hr_test,statistic = linear.function, R =1000, formula = satisfaction_level~.)

predicted.values <- lin.result$t0
#median.predicted.values <- ifelse(predicted.values >= 0.65,0,1)

#1 - mean(median.predicted.values != median.left.score)

