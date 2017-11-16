rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data")
getwd()
require(class)

#Data Loading
hrData <- read.delim("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data/HR_comma_sep.csv", sep = ",", header= TRUE)
pairs(hrData)
attach(hrData)

#By salary
highSal = subset(hrData, salary == "high")
medSal = subset(hrData, salary =="medium")
lowSal = subset(hrData, salary == "low")

summary(highSal)
summary(lowSal)
#Number of people left in per salary band
nrow(subset(highSal, left ==1)) #82/1237 6.66%
nrow(subset(medSal, left ==1)) #1317/6446 20.43%
nrow(subset(lowSal, left ==1)) #2127/7316 29.07%


cor(hrData[,1:8])

pairs(highSal)
pairs(medSal)
pairs(lowSal)

#By work accident
nrow(subset(highSal, Work_accident ==1)) # 192
nrow(subset(medSal, Work_accident ==1)) #937
nrow(subset(lowSal, Work_accident ==1)) #1040

#To check whether employee satisfication level is sufficicent to predict whether employee will leave or not 
sfLvl_left = data.frame(hrData$left,hrData$satisfaction_level)
sfLvl_left$hrData.predLeft =  ifelse(sfLvl_left$hrData.satisfaction_level < 0.5,1,0)
sfLvl_left$hrData.satisfaction_level = NULL

cor(sfLvl_left)
require(utils)

mean(sfLvl_left$hrData.left != sfLvl_left$hrData.predLeft) #20% error
