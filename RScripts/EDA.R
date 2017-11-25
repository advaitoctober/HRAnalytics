rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data")
getwd()
require(class)
library(ggplot2)

#Data Loading
hrData <- read.delim("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data/HR_comma_sep.csv", sep = ",", header= TRUE)
#pairs(hrData)
attach(hrData)

#By salary
highSal = subset(hrData, salary == "high")
medSal = subset(hrData, salary =="medium")
lowSal = subset(hrData, salary == "low")

summary(highSal)
summary(lowSal)
#Number of people left in per salary band
vSal = c('low','meduim','high')
vTotPop = c(nrow(lowSal),nrow(medSal),nrow(highSal))
vSalLeft = c(nrow(subset(lowSal, left ==1)),nrow(subset(medSal, left ==1)),nrow(subset(highSal, left ==1)) )
salDF = data.frame(Salary=vSal,TotalEmployees = vTotPop, EmployeesLeft=vSalLeft)
salDF["PercentLeft"] = (salDF["EmployeesLeft"]/salDF["TotalEmployees"])*100
salDF["EmployeesStayed"] = (salDF["TotalEmployees"]-salDF["EmployeesLeft"])
barplot(salDF$PercentLeft,names.arg = salDF$Salary,ylab = "Attrition Percent",xlab = "Salary")



############################
#By Promotion last 5 year
############################
plot(hrData$promotion_last_5years)
promo1Left = nrow(subset(hrData,promotion_last_5years ==1 & left==1))
promo0Left = nrow(subset(hrData,promotion_last_5years ==0 & left==1))
promo1Stay = nrow(subset(hrData,promotion_last_5years ==1 & left==0))
promo0Stay = nrow(subset(hrData,promotion_last_5years ==0 & left==0))

promoDF = data.frame(Promotion = c("Promoted","Promoted","NotPromoted","NotPromoted") ,Left = c("Stayed","Left","Stayed","Left"),NoOfEmployees = c(promo1Stay,promo1Left,promo0Stay,promo0Left))


ggplot(promoDF, aes(Promotion,NoOfEmployees , fill = Left)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")


summary(hrData$last_evaluation)

plot(hrData$last_evaluation[which(left==0)])

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
