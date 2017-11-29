rm(list = ls())
#setwd("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data")
setwd("C:/DragonBallZ/git_Repo/HRAnalytics/HRAnalytics/RScripts")
getwd()
require(class)
library(ggplot2)
require(reshape2)

#Data Loading
#hrData <- read.delim("/Users/varadtupe/Documents/GitHub/HRAnalytics/Data/HR_comma_sep.csv", sep = ",", header= TRUE)
hrData <- read.delim("C:/DragonBallZ/git_Repo/HRAnalytics/HRAnalytics/RScripts/HR_comma_sep.csv", sep = ",", header= TRUE)
#pairs(hrData)
attach(hrData)

############################
#By salary
############################

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
barplot(salDF$PercentLeft,names.arg = salDF$Salary,ylab = "Attrition Percent",xlab = "Salary",main="Salary vs Attrition %")

############################
#By Promotion last 5 year
############################
plot(hrData$promotion_last_5years)
hist(hrData$promotion_last_5years,xlab = "Whether received a Promotion or not" , ylab = "Freq")

promo1Left = nrow(subset(hrData,promotion_last_5years ==1 & left==1))
promo0Left = nrow(subset(hrData,promotion_last_5years ==0 & left==1))
promo1Stay = nrow(subset(hrData,promotion_last_5years ==1 & left==0))
promo0Stay = nrow(subset(hrData,promotion_last_5years ==0 & left==0))

promoDF = data.frame(Promotion = c("Promoted","Promoted","NotPromoted","NotPromoted") ,Left = c("Stayed","Left","Stayed","Left"),NoOfEmployees = c(promo1Stay,promo1Left,promo0Stay,promo0Left))

ggplot(promoDF, aes(Promotion,NoOfEmployees , fill = Left)) + 
  geom_bar(stat="identity", position = "dodge")

slices <- c(promo1Left, promo0Left,promo1Stay, promo0Stay)
lbls <- c("Promoted and Left", "Not Promoted and Left", "Promoted and Stayed", "Not Promoted and Stayed")
pie(slices, labels = lbls, main="Representation of People")

############################
#By Department
############################
depDF = data.frame(aggregate(left~department,data = hrData,FUN = length))
depDF
colnames(depDF) = c("Department","TotalEmployees")
depDF$Left = (aggregate(left~department,data = hrData,FUN = sum))[,2]
depDF$Stayed = depDF$TotalEmployees - depDF$Left
depDF$Attrition = (depDF$Left / depDF$TotalEmployees) * 100
depDFMelt = melt(depDF[,c("Department","Stayed","Left")])


ggplot(depDFMelt,aes(x = Department,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")

ggplot(depDF,aes(x = Department,y = Attrition)) +  
  geom_bar(aes(fill = Attrition),stat = "identity",position = "dodge")

################################
#Satisfaction Level
################################
satDF = hrData[,c("satisfaction_level","left")]
satDF$sat_category = ifelse(satDF$satisfaction_level>= 0.8,"Very High","")
satDF$sat_category = ifelse(satDF$satisfaction_level>= 0.6 & satDF$satisfaction_level< 0.8,"High","")
satDF$sat_category = ifelse(satDF$satisfaction_level>= 0.4 & satDF$satisfaction_level< 0.6,"Medium","")
satDF$sat_category = ifelse(satDF$satisfaction_level>= 0.2 & satDF$satisfaction_level< 0.4,"Low","")
satDF$sat_category = ifelse(satDF$satisfaction_level< 0.2,"Very Low","")


satDF$sat_category = as.factor(ifelse(satDF$satisfaction_level>= 0.8,"Very High",ifelse(satDF$satisfaction_level>= 0.6 & satDF$satisfaction_level< 0.8,"High",ifelse(satDF$satisfaction_level>= 0.4 & satDF$satisfaction_level< 0.6,"Medium",ifelse(satDF$satisfaction_level>= 0.2 & satDF$satisfaction_level< 0.4,"Low",ifelse(satDF$satisfaction_level< 0.2,"Very Low",""))))))

satAggDF = data.frame(aggregate(left~sat_category,data = satDF,FUN = length))
colnames(satAggDF) = c("SatisfactionCategory","TotalEmployees")
satAggDF$Left = (aggregate(left~sat_category,data = satDF,FUN = sum))[,2]

satAggDF$Stayed = satAggDF$TotalEmployees - satAggDF$Left
satAggDF$Attrition = (satAggDF$Left / satAggDF$TotalEmployees) * 100


satDFMelt = melt(satAggDF[,c("SatisfactionCategory","Stayed","Left")])


ggplot(satDFMelt,aes(x = SatisfactionCategory,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")+
  scale_x_discrete(limits=c("Very Low","Low","Medium","High","Very High"))

ggplot(satAggDF,aes(x = SatisfactionCategory,y = Attrition)) + 
  geom_bar(aes(fill = Attrition),stat = "identity",position = "dodge")+
  scale_x_discrete(limits=c("Very Low","Low","Medium","High","Very High"))+
  scale_colour_manual(values = rev(brewer.pal(3,"BuPu")))


#plot(hrData$last_evaluation[which(left==0)])

#cor(hrData[,1:8])

################################
#By work accident
################################
nrow(subset(highSal, Work_accident ==1)) # 192
nrow(subset(medSal, Work_accident ==1)) #937
nrow(subset(lowSal, Work_accident ==1)) #1040

################################
#To check whether employee satisfication level is sufficicent to predict whether employee will leave or not 
################################
sfLvl_left = data.frame(hrData$left,hrData$satisfaction_level)
sfLvl_left$hrData.predLeft =  ifelse(sfLvl_left$hrData.satisfaction_level < 0.5,1,0)
sfLvl_left$hrData.satisfaction_level = NULL

cor(sfLvl_left)
require(utils)

mean(sfLvl_left$hrData.left != sfLvl_left$hrData.predLeft) #20% error


################################
#Number of years spent in the company
################################
timespent_leftstay = data.frame(hrData$time_spend_company,hrData$left)
timespent_left <- subset(timespent_leftstay , timespent_leftstay$hrData.left == 1)
avg_time_spent_before_leaving <- mean(timespent_left$hrData.time_spend_company)
#calc average , min and max is inconsequential
avg_time_spent_before_leaving
min(timespent_left$hrData.time_spend_company)
max(timespent_left$hrData.time_spend_company)

timespent_stay <- subset(timespent_leftstay , timespent_leftstay$hrData.left == 0)
avg_time_spent_if_stay <- mean(timespent_stay$hrData.time_spend_company)
#calc average , min and max is inconsequential
avg_time_spent_if_stay
min(timespent_stay$hrData.time_spend_company)
max(timespent_stay$hrData.time_spend_company)

################################
#Average monthly hours at workplace
################################        
avgmhrs_leftstay = data.frame(hrData$average_montly_hours,hrData$left)

avgmhrs_left <- subset(avgmhrs_leftstay , avgmhrs_leftstay$hrData.left == 1)
avg_monthlyhrs_spent_before_leaving <- mean(avgmhrs_left$hrData.average_montly_hours)
avg_monthlyhrs_spent_before_leaving
min(avgmhrs_left$hrData.average_montly_hours)
max(avgmhrs_left$hrData.average_montly_hours)

avgmhrs_stay <- subset(avgmhrs_leftstay , avgmhrs_leftstay$hrData.left == 0)
avg_monthlyhrs_if_stay <- mean(avgmhrs_stay$hrData.average_montly_hours)
avg_monthlyhrs_if_stay
min(avgmhrs_stay$hrData.average_montly_hours)
max(avgmhrs_stay$hrData.average_montly_hours)

##########################################
#No. of Projects
##########################################
summary(hrData$number_project)
noProj = data.frame(aggregate(left~number_project,data = hrData,FUN = length))
colnames(noProj) = c("No_Of_Project","TotalEmployees")
noProj$Left = (aggregate(left~number_project,data = satDF,FUN = sum))[,2]
noProj$Stayed = noProj$TotalEmployees - noProj$Left
noProj$Attrition = (noProj$Left / noProj$TotalEmployees) * 100
noProj$No_Of_Project = as.factor(noProj$No_Of_Project)

#plot(noProj$No_Of_Project,noProj$Attrition)

noProjMelt = melt(noProj[,c("No_Of_Project","Stayed","Left")])

ggplot(noProjMelt,aes(x = No_Of_Project,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge")


ggplot(noProj,aes(x=No_Of_Project, y=Attrition , group = 1)) +
  geom_line(aes(color=Attrition)) +
  geom_point(aes(color=Attrition))

