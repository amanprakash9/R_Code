#setwd("C:/Users/OM/Downloads/UGIITB HR case study logistic/PA-I_Case_Study_HR_Analytics")
#HR case study to find out the main reasons for attrition of employees
#add data dictionary

######################################################
#Business Objective
######################################################
#To identify variables which are strong indicators of an employee leaving the
#organization.
#
#####################
#Data understanding
#####################
#Age-numeric-	Age of the employee
#Attrition-category(dependent variable)-	Whether the employee left in the previous year or not
#BusinessTravel	-category -How frequently the employees travelled for business purposes in the last year
#Department	-category- Department in company	
#DistanceFromHome-numeric-	Distance from home in kms
#Education- category-	Education Level	1 'Below College',2 'College',3 'Bachelor',4 'Master',5 'Doctor'
#EducationField	-category- Field of education	
#EmployeeNumber-numeric-key-	Employee number/id	
#EnvironmentSatisfaction- category-	Work Environment Satisfaction Level	1 'Low'
#2 'Medium',3 'High',4 'Very High'
#Gender-category	Gender of employee	
#JobInvolvement-category-	Job Involvement Level	1 'Low',2 'Medium',3 'High',4 'Very High'
#JobLevel	-category -Job level at company on a scale of 1 to 5	
#JobRole	-category- Name of job role in company	
#JobSatisfaction	-category- Job Satisfaction Level	1 'Low',2 'Medium',3 'High',4 'Very High'
#MaritalStatus- category-	Marital status of the employee	
#MonthlyIncome-numeric-	Monthly income in rupees per month	
#NumCompaniesWorked-numeric-	Total number of companies the employee has worked for	
#PercentSalaryHike-numeric-	Percent salary hike for last year	
#PerformanceRating-category-	Performance rating for last year	1 'Low'
#2 'Good',3 'Excellent',4 'Outstanding'
#RelationshipSatisfaction-category-	Relationship satisfaction level	1 'Low'
#2 'Medium',3 'High',4 'Very High'
#StandardHours-numeric	-Standard hours of work for the employee	
#StockOptionLevel	-category-Stock option level of the employee	
#TotalWorkingYears-numeric-	Total number of years the employee has worked so far	
#TrainingTimesLastYear-numeric-	Number of times training was conducted for this employee last year	
#WorkLifeBalance	-category- Work life balance level	1 'Bad'
#2 'Good',3 'Better',4 'Best'
#YearsAtCompany	-numeric-Total number of years spent at the company by the employee	
#YearsSinceLastPromotion-numeric-	Number of years since last promotion	
#YearsWithCurrManager-numeric-	Number of years under current manager	

##############################
#################################
#load library list              #
#################################
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(VIM)
library(reshape2)
library(car)
#library(MASS)
library(e1071)
library(ROCR)

load.libraries <- c('data.table', 'e1071', 'stringr','lubridate', 'caret', 'VIM', 'reshape2','dplyr','tidyr','e1071','ROCR','car')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

##################################
#import data sets into dataframes#
##################################
#gd- general data
#esd- employee survey data
#msd- manager survey data

msd=read.csv("manager_survey_data.csv",stringsAsFactors = F, na.strings= c("?",""," ","NA","n/a"))
esd=read.csv("employee_survey_data.csv",stringsAsFactors = F, na.strings= c("?",""," ","NA","n/a"))
gd=read.csv("general_data.csv",stringsAsFactors = F, na.strings= c("?",""," ","NA","n/a"))

intime=read.csv("in_time.csv",stringsAsFactors = F, na.strings= c("?",""," ","NA","n/a"))
outime=read.csv("out_time.csv",stringsAsFactors = F, na.strings= c("?",""," ","NA","n/a"))

#################################
#merge data from msd, esd and gd#
#################################
#all the keys are matching before merging data sets
nrow(distinct(gd, EmployeeID))
#4410
nrow(distinct(msd, EmployeeID))
#4410

if (sum(msd$EmployeeID %in% gd$EmployeeID) == nrow(gd))
{
  print('all employee data present in msd , no isssues')
}
#display no of cols before and after merge
ncol(gd)
ncol(msd)
gd1=merge(gd,msd,by="EmployeeID")
ncol(gd1)
#
nrow(distinct(esd, EmployeeID))
#4410
ncol(esd)
if (sum(esd$EmployeeID %in% gd$EmployeeID) == nrow(gd))
{
  print('all employee data present in esd, no isssues')
}
gd1=merge(gd1,esd,by="EmployeeID")
ncol(gd1)

#sanity check on intime and outime 
colSums(is.na(intime))
colSums(is.na(outime))
#how many columns have all null values
sum(colSums(is.na(intime))==4410)
#12
sum(colSums(is.na(outime))==4410)
#12
#so it seems when intime is not available corresponding outime is also not available
#these may be holiday or employee is on leave
#we are not treating NA values because they are valid in this scenario when
#employee is absent so that should not be counted while calculating mean average hours
#for an employee

colnames(intime)[1] <- "EmployeeID"
colnames(outime)[1] <- "EmployeeID"

#standarise in and out time
intime1=data.frame(lapply(intime[,2:262],ymd_hms))
outime1=data.frame(lapply(outime[,2:262],ymd_hms))
##
#find the working hours based on in and out time difference
#
time_diff=outime1[,2:261]-intime1[,2:261]
time_diff=data.frame(apply(time_diff,2,as.character))
#Second argument 1 indicated Processing along rows .
#if it is 2 then it indicated processing along the columns
#summary(time_diff)
time_diff=data.frame(apply(time_diff,2,function (x) str_extract(x,"[0-9.]+")))
time_diff=data.frame(apply(time_diff,2,as.numeric))

#sanity check
sum(time_diff < 0, na.rm=T)
#
avg_working_hour=data.frame(avg_work_hour=rowMeans(time_diff,na.rm = T))
avg_working_hour$EmployeeID=intime$EmployeeID
summary(avg_working_hour)

#calculating leave each employee took by subtracting the holidays.

table(colSums(is.na(time_diff))==4410)
nol=rowSums(is.na(time_diff))-11
avg_working_hour$no_of_leave=nol

#create master data frame after merging all the input data sets
nrow(distinct(avg_working_hour, EmployeeID))
#4410

if (sum(avg_working_hour$EmployeeID %in% gd$EmployeeID) == nrow(gd))
{
  print('all employee data present in avg_working_hour, no isssues')
}
ncol(avg_working_hour)
master_merged=merge(gd1,avg_working_hour,by= "EmployeeID")
ncol(master_merged)
#########
#check structure and summary of master data frame having all details
#########
str(master_merged)
summary(master_merged)
colSums(is.na(master_merged))
#since there are very few NAs and only in couple of columns so it will
#not impact the overall analysis
# 
missing_values <- master_merged %>%summarise_all(funs(sum(is.na(.))/n()))
missing_values
#missing value percentage is very low, these can be removed
table(duplicated(master_merged))
#no duplicate rows
x=colSums(is.na(master_merged))
x
y=names(x[x>.30*nrow(master_merged)])
y
#no such column having more than 30% NAs

########
#remove unncessary columns not required for analysis
#########
#remove employee id column
master_merged <- master_merged[,-1]

#find columns with near zero variance

badCols <- nearZeroVar(master_merged)
print(paste("Fraction of nearZeroVar columns:", round(length(badCols)/length(master_merged),4)))
summary(master_merged[,c(8,15,17)])
# only one value in 8th columni.e  1, 15th has only 'Y' and 17th only 8
##removing these columns which dont have much variance as they are not good for analysis
master_merged=master_merged[,-badCols]
dim(master_merged)

##############
#imputation  #
#############


master=kNN(master_merged)
master=master[,c(1:27)]

colSums(is.na(master))


#############
#treating different columns so they are in correct format
############

#converting categorical variables into factors
master$Attrition <- as.factor(master$Attrition)
summary(master$Attrition)

master$BusinessTravel <- as.factor(master$BusinessTravel)
summary(master$BusinessTravel)

master$Department <- as.factor(master$Department)
summary(master$Department)

master$Education<- as.factor(master$Education)
summary(master$Education)

master$EducationField<- as.factor(master$EducationField)
summary(master$EducationField)

master$Gender <- as.factor(master$Gender)
summary(master$Gender)


master$JobLevel <- as.factor(master$JobLevel)
summary(master$JobLevel)

master$JobRole <- as.factor(master$JobRole)
summary(master$JobRole)

master$MaritalStatus <- as.factor(master$MaritalStatus)
summary(master$MaritalStatus)

master$StockOptionLevel <- as.factor(master$StockOptionLevel)
summary(master$StockOptionLevel)

master$JobInvolvement<- as.factor(master$JobInvolvement)
summary(master$JobInvolvement)

master$PerformanceRating<- as.factor(master$PerformanceRating)
summary(master$PerformanceRating)

master$EnvironmentSatisfaction <- as.factor(master$EnvironmentSatisfaction)
summary(master$EnvironmentSatisfaction)

master$JobSatisfaction<- as.factor(master$JobSatisfaction)
summary(master$JobSatisfaction)
#master$RelationshipSatisfaction<- as.factor(master$Relat)
#summary(master$RelationshipSatisfaction)

master$WorkLifeBalance<- as.factor(master$WorkLifeBalance)
summary(master$WorkLifeBalance)

#
#
#sapply(master, is.numeric)
#################
#remove outliers#
#################
summary(master)

#check outlier in DistanceFromHome
quantile(master$DistanceFromHome, seq(0,1,0.01))
#this looks fine no outlier

summary(master$MonthlyIncome)
#check outlier in Monthly Income
quantile(master$MonthlyIncome, seq(0,1,0.01))
#Note that there is a jump from 1 to 2%. so floor all the values below 18590(2%) to 
# to 18590 also there is a big jup from 95 to 96 so cap all the values above 171740.0
#to 171740.0(94%)
master$MonthlyIncome[which(master$MonthlyIncome < 18590.0)] <- 18590.0
master$MonthlyIncome[which(master$MonthlyIncome > 171740.0)] <- 171740.0
summary(master$MonthlyIncome)

#check outlier in No of companies worked
quantile(master$NumCompaniesWorked, seq(0,1,0.01), na.rm=T)
#this looks fine no outlier

#check outlier in Percentsalaryhike
quantile(master$PercentSalaryHike, seq(0,1,0.01))
#this looks fine no outlier
#

#check outliers in TotalWorkingYears
#
summary(master$TotalWorkingYears)
quantile(master$TotalWorkingYears, seq(0,1,0.01))
#Note that there is a jump from 99 to 100%. so cap all the values above 35(99%)
#to 35
 
master$TotalWorkingYears[which(master$TotalWorkingYears > 35)] <- 35
summary(master$TotalWorkingYears)

#check outlier in TrainingTimelastyear
quantile(master$TrainingTimesLastYear, seq(0,1,0.01))
#this looks fine no outlier

#check outlier in YearsatCompany
quantile(master$YearsAtCompany, seq(0,1,0.01))
#Note that there is a jump from 98 to 99%. so cap all the values above 24(98%)
#to 24

master$YearsAtCompany[which(master$YearsAtCompany > 24)] <- 24
summary(master$YearsAtCompany)

#check outlier in YearsSinceLastPromotion
quantile(master$YearsSinceLastPromotion, seq(0,1,0.01))
#this looks fine no outlier

#check outlier in Yearswithcurrentmanager
quantile(master$YearsWithCurrManager, seq(0,1,0.01))
#there is a jump from 99 to 100%ile, so cap all the values above 14(99%) to 14

master$YearsWithCurrManager[which(master$YearsWithCurrManager > 14)] <- 14
summary(master$YearsWithCurrManager)

#check outlier in noofleaves
quantile(master$no_of_leave, seq(0,1,0.01))
#this looks fine no outlier



#check outlier in quantile(master$TotalWorkingYears, 
#another way
#outlier_values <- boxplot.stats(master$MonthlyIncome)$out
#
#boxplot(master$MonthlyIncome, main="Monthly Income", boxwex=0.1)
#mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

####################################
#EDA
####################################
#############################
master$Attrition=if_else(master$Attrition=="Yes",1,0)
cor(master$Age,master$Attrition)
# common hypothesis  is as people tends to get old they dont really switch companies a lot
#  more or less from stability point of view or they are accustomes to the lifestyle..

ggplot(master,aes(BusinessTravel,fill=as.factor(Attrition)))+geom_bar(position = "fill")
# people who trave frequently switch more.

ggplot(master,aes(Department,fill=as.factor(Attrition)))+geom_bar(position = "fill")
# hr dept. switch more

cor(master$DistanceFromHome,master$Attrition)
# no relation

#ggplot(master,aes(Age,MonthlyIncome,color=as.factor(Attrition)))+geom_point()
ggplot(data=master, aes(Age, fill=as.factor(Attrition))) + geom_histogram(binwidth = 1)
#employees within age group 25-35 tend to switch more
#<= 25 1
#>25 and <=35 2
#>35 and <= 45 3
#>45 4

master$agegrp[master$Age <= 25] <- 1
master$agegrp[master$Age > 25 & master$Age <= 35] <- 2
master$agegrp[master$Age > 35 & master$Age <= 45] <- 3
master$agegrp[master$Age > 45] <- 4
master$agegrp <- as.factor(as.numeric(master$agegrp))
summary(master$agegrp)
ggplot(master, aes(x=agegrp, fill=Attrition)) + geom_bar()
#age group 2,3  tend to switch more i.e persons between 25 and 45

ggplot(master,aes(Education,fill=as.factor(Attrition)))+geom_bar(position = "fill")
# employee educated till college switch more

ggplot(master,aes(EducationField,fill=as.factor(Attrition)))+geom_bar(position = "fill")
# again human resources field tend to switch more

ggplot(master,aes(Gender,fill=as.factor(Attrition)))+geom_bar(position = "fill")
#no rel

ggplot(master,aes(JobLevel,fill=as.factor(Attrition)))+geom_bar(position = "fill")
prop.table(table(master$JobLevel,master$Attrition),1)
# job level 2 & 4 switch more

ggplot(master,aes(JobRole,fill=as.factor(Attrition)))+geom_bar(position = "fill") +theme(axis.text.x=element_text(angle=90, hjust=1))
#research director switch highest and manufacturing director least..

ggplot(master,aes(MaritalStatus,fill=as.factor(Attrition)))+geom_bar(position = "fill")
#single people switch more


ggplot(master,aes(NumCompaniesWorked,fill=as.factor(Attrition)))+geom_bar(position = "fill")
ggplot(data=master, aes(as.factor(NumCompaniesWorked), fill=as.factor(Attrition))) + geom_histogram(stat="count", position = "fill")

#slight relation , employee with more than 4 companies worked are more likely to switch

master%>%
  group_by(Attrition)%>%
  summarise(median(PercentSalaryHike))

ggplot(master,aes(PercentSalaryHike,fill=as.factor(Attrition)))+geom_bar(position = "fill")
# 25% salary hike tend to switch

ggplot(master,aes(StockOptionLevel,fill=as.factor(Attrition)))+geom_bar(position = "fill")
#no rel


master%>%
  group_by(Attrition)%>%
  summarise(median(TotalWorkingYears,na.rm =T))
# less total working year switch more



ggplot(master,aes(TrainingTimesLastYear,fill=as.factor(Attrition)))+geom_bar(position = "fill")

#a bit of a trend as people with less training tend to switch


ggplot(master,aes(YearsAtCompany,fill=as.factor(Attrition)))+geom_bar(position = "fill")

master%>%
  group_by(Attrition)%>%
  summarise(median(YearsAtCompany,na.rm =T))

#years at company is imp as people who have invested a lot of years generally dont switch.
#there is a peak at 24 which suggests that there may be not be enough opportunity 
#for some people and they then look for switch

ggplot(master,aes(YearsSinceLastPromotion,fill=as.factor(Attrition)))+geom_bar(position = "fill")

master%>%
  group_by(Attrition)%>%
  summarise(median(YearsSinceLastPromotion,na.rm =T))

table(master$YearsSinceLastPromotion,master$Attrition)

########no rel

ggplot(master,aes(YearsWithCurrManager,fill=as.factor(Attrition)))+geom_bar(position = "fill")

master%>%
  group_by(Attrition)%>%
  summarise(median(YearsWithCurrManager,na.rm =T))

table(master$YearsWithCurrManager)
##  attrition is less if u have stayed with current manager for a long period of time.

ggplot(master,aes(JobInvolvement,fill=as.factor(Attrition)))+geom_bar(position = "fill")

#attrition a bit high if job involvement is low

ggplot(master,aes(PerformanceRating,fill=as.factor(Attrition)))+geom_bar(position = "fill")

#master%>%
#group_by(EnvironmentSatisfaction,Attrition)%>%
#  summarise(total=n())
prop.table(table(master$EnvironmentSatisfaction,master$Attrition),1)
#least environment statisfaction are more likely to leave

ggplot(master,aes(EnvironmentSatisfaction,fill=as.factor(Attrition)))+geom_bar(position = "fill")

#high attrition if EnvironmentSatisfaction is 1

ggplot(master,aes(JobSatisfaction,fill=as.factor(Attrition)))+geom_bar(position = "fill")

#a good rel as job sat is low attrition is high

ggplot(master,aes(WorkLifeBalance ,fill=as.factor(Attrition)))+geom_bar(position = "fill")
#a good rel as worklifebal is low attrition is high

master%>%
  group_by(Attrition)%>%
  summarise(median(avg_work_hour))
#people who switch have there working hours high..



ggplot(master,aes(no_of_leave,fill=as.factor(Attrition)))+geom_bar(position = "fill")

master%>%
  group_by(Attrition)%>%
  summarise(median(no_of_leave))
# people who switch take less leave


########################
#Bi-variate analysis   #
########################
master_num_col <- sapply(master, is.numeric)
master_num <- master[,master_num_col]


#Finding higly correlated features 
highly.cor <- findCorrelation(cor(master_num), cutoff=0.85)
# 
names(master_num[,highly.cor])

#Compute the correlation matrix
cormat <- round(cor(master_num, use="na.or.complete"), digits=2)
head(cormat)

#Create the correlation heatmap with ggplot2
#The package reshape is required to melt the correlation matrix :

#library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

#The function geom_tile()[ggplot2 package] is used to visualize the correlation
#matrix :
#library(ggplot2)
ggheatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#good  corelation between yearsincompany and years with manager
#years at company total work years
#total work years and age


#######
#attrition rate
###
Attrition<- sum(master$Attrition)/nrow(master)
Attrition # 16.12% attrition rate. 
master$Attrition <- as.factor(master$Attrition)
##################################
#Feature standardization
##################################
# Feature standardisation

# Normalising continuous features 
master1 <- master %>%  mutate_if(is.numeric, scale)


#################################
#Create dummy variables
#################################

#master$Attrition=if_else(master$Attrition=="Yes",1,0)
dummy=function(x,y){
  z=model.matrix(~x,y)
  zz=z[,2:ncol(z)]
  cbind(y,zz)
}

master1=dummy(master1$BusinessTravel,master1)

master1=dummy(master1$Department,master1)

master1=dummy(master1$Education,master1)
colnames(master1)[33:36] <- c("xCollege","xBachelor","xMaster","xDoctor")

master1=dummy(master1$EducationField,master1)

master1=dummy(master1$Gender,master1)
colnames(master1)[42] <- "xGender"

master1=dummy(master1$JobLevel,master1)
colnames(master1)[43:46] <- c("xJL2","xJL3","xJL4","xJL5")

master1=dummy(master1$JobRole,master1)

master1=dummy(master1$MaritalStatus,master1)

master1=dummy(master1$JobInvolvement,master1)
colnames(master1)[57:59] <- c("xJIMedium","xJIHigh","xJIVery High")

##PR - 0==3 and 1==4
master1=dummy(master1$PerformanceRating,master1)
colnames(master1)[60] <- "xPerformanceRating"

master1=dummy(master1$EnvironmentSatisfaction,master1)
colnames(master1)[61:63] <- c("xESMedium","xESHigh","xESVery High")

master1=dummy(master1$JobSatisfaction,master1)
colnames(master1)[64:66] <- c("xJSMedium","xJSHigh","xJSVery High")

master1=dummy(master1$WorkLifeBalance,master1)
colnames(master1)[67:69] <- c("xWLBGood","xWLBBetter","xWLBBest")

master1=dummy(master1$StockOptionLevel,master1)
colnames(master1)[70:72] <- c("xSOL1","xSOL2","xSOL3")

master1=dummy(master1$agegrp,master1)
colnames(master1)[73:75] <- c("xAgeGT25&LE35","xAgeGT35&LE45","xAgeGT45")
 

master1=select(master1,-c(BusinessTravel,Department,Education,EducationField,Gender,
                          JobRole,JobLevel,MaritalStatus,JobInvolvement,PerformanceRating,
                          EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,agegrp,
                          StockOptionLevel))

str(master1)
summary(master1)
View(master1)
#4410 observations of 68 variables

########################################################################
# splitting the data between train and test
set.seed(100)

trainindices= sample(1:nrow(master1), 0.7*nrow(master1))
train = master1[trainindices,]
test = master1[-trainindices,]

summary(train)
str(train)
########################################################################
# Logistic Regression: 
#library(car)
#Initial model
model_1 = glm( Attrition~ ., data = train, family = "binomial")
summary(model_1) #AIC 3015.1.....nullDev 3895.7...resDev 2905.1

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)


#model_3 after removing xLife Sciences which has highest vif 17.102203

model_3 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
      YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
      xTravel_Rarely + `xResearch & Development` + xSales + xDoctor + 
       + xMarketing + xMedical + xOther + `xTechnical Degree` + 
      xJL5 + xManager + `xManufacturing Director` + `xResearch Director` + 
      `xSales Executive` + `xSales Representative` + xSingle + 
      xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
      xJSHigh + `xJSVery High` + xWLBGood + xWLBBetter + xWLBBest + 
      xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
    data = train)

summary(model_3)
vif(model_3)

#xTravel_Frequently which has highest VIF values 4.880840 is very significant
#also next xTravel_Rarely  4.832273
#since these two are highly co-related , we can remove xTravel_Rarely as it is
#less signification
cor(master1$xTravel_Frequently,master1$xTravel_Rarely)
#-0.75

#create model 4 after removing xTravel_Rarely
model_4 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                `xResearch & Development` + xSales + xDoctor + 
                + xMarketing + xMedical + xOther + `xTechnical Degree` + 
                xJL5 + xManager + `xManufacturing Director` + `xResearch Director` + 
                `xSales Executive` + `xSales Representative` + xSingle + 
                xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                xJSHigh + `xJSVery High` + xWLBGood + xWLBBetter + xWLBBest + 
                xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
              data = train)  

summary(model_4)
vif(model_4)

# both xSales and xResearch & Development have very high VIF and are very significant
cor(master1$xSales, master1$`xResearch & Development`)
#these two are highly corelated
#-0.91
#creating model5 after removing xSales 
model_5 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                `xResearch & Development` + xDoctor + 
                + xMarketing + xMedical + xOther + `xTechnical Degree` + 
                xJL5 + xManager + `xManufacturing Director` + `xResearch Director` + 
                `xSales Executive` + `xSales Representative` + xSingle + 
                xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                xJSHigh + `xJSVery High` + xWLBGood + xWLBBetter + xWLBBest + 
                xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
              data = train)  

summary(model_5)
vif(model_5)

#vif of both xWLBGood and xWLBBetter is very high and both are very significant
#also corelation is very good
cor(master1$xWLBGood,master1$xWLBBetter)
#-0.69
#create next model after removing xWLBBetter

model_6 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                `xResearch & Development` + xDoctor + 
                + xMarketing + xMedical + xOther + `xTechnical Degree` + 
                xJL5 + xManager + `xManufacturing Director` + `xResearch Director` + 
                `xSales Executive` + `xSales Representative` + xSingle + 
                xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                xJSHigh + `xJSVery High` + xWLBGood + xWLBBest + 
                xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
              data = train)  

summary(model_6)
vif(model_6)

#next ones are age and totalworkingyears and both are very significant
#and there is good corelation between them
cor(master1$Age, master1$TotalWorkingYears)
#0.68

#create next model after removing TotalWorkingYears
model_7 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                `xResearch & Development` + xDoctor + 
                + xMarketing + xMedical + xOther + `xTechnical Degree` + 
                xJL5 + xManager + `xManufacturing Director` + `xResearch Director` + 
                `xSales Executive` + `xSales Representative` + xSingle + 
                xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                xJSHigh + `xJSVery High` + xWLBGood + xWLBBest + 
                xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
              data = train)  

summary(model_7)
vif(model_7)
 

#now since all the VIF values are less than 2 which is very good
#let us try to remove the insignificant variables based on p value

#create next model after removing xWLBBest,xWLBGood  which has highest p values 0.959
model_8 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                `xResearch & Development` + xDoctor + 
                + xMarketing + xMedical + xOther + `xTechnical Degree` + 
                xJL5 + xManager + `xManufacturing Director` + `xResearch Director` + 
                `xSales Executive` + `xSales Representative` + xSingle + 
                xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                xJSHigh + `xJSVery High` + 
                xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
              data = train)  

summary(model_8)
 
#create next model after removing least significant `xSales Executive`  column
model_9 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                `xResearch & Development` + xDoctor + 
                + xMarketing + xMedical + xOther + `xTechnical Degree` + 
                xJL5 + xManager + `xManufacturing Director` + `xResearch Director` + 
                 `xSales Representative` + xSingle + 
                xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                xJSHigh + `xJSVery High` + 
                xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
              data = train)  

summary(model_9)

#create next model after removing `xResearch Director`
#which is least significant
model_10 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                `xResearch & Development` + xDoctor + 
                + xMarketing + xMedical + xOther + `xTechnical Degree` + 
                xJL5 + xManager + `xManufacturing Director` + 
                `xSales Representative` + xSingle + 
                xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                xJSHigh + `xJSVery High` + 
                xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
              data = train)  

summary(model_10)

#create next model after removing xOther, xMedical
model_11 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 `xResearch & Development` + xDoctor + 
                 + xMarketing + `xTechnical Degree` + 
                 xJL5 + xManager + `xManufacturing Director` + 
                 `xSales Representative` + xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_11)

#create next model after removing `xTechnical Degree` base on p value
model_12 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 `xResearch & Development` + xDoctor + 
                 + xMarketing + 
                 xJL5 + xManager + `xManufacturing Director` + 
                 `xSales Representative` + xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_12)

#create next model after removing xMarketing based on p value
model_13 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 `xResearch & Development` + xDoctor + 
                 xJL5 + xManager + `xManufacturing Director` + 
                 `xSales Representative` + xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_13)

#create next model after removig `xResearch & Development`
#base on p value

model_14 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                  xDoctor + 
                 xJL5 + xManager + `xManufacturing Director` + 
                 `xSales Representative` + xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_14)

#create next model after removing xJL5
#based on p value
model_15 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xDoctor + xManager + `xManufacturing Director` + 
                 `xSales Representative` + xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 xSOL1 + `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_15)

#create next model after removing xSOL1 
#based on p value
model_16 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xDoctor + xManager + `xManufacturing Director` + 
                 `xSales Representative` + xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                  `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_16)

#create next model after removing `xSales Representative`
#based on p value
model_17 = glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xDoctor + xManager + `xManufacturing Director` + 
                  xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_17)

#create next model after removing MonthlyIncome
#based on p value
model_18 = glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xDoctor + xManager + `xManufacturing Director` + 
                 xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 `xAgeGT25&LE35` + `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_18)

#create next model after removing `xAgeGT25&LE35`
#based on p value
model_19 = glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xDoctor + xManager + `xManufacturing Director` + 
                 xSingle + 
                 xJIHigh + xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                  `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_19)

#create next model after removing xJIHigh based on p values
model_20 = glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xDoctor + xManager + `xManufacturing Director` + 
                 xSingle + 
                 xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_20)

#create next model after removing xDoctor based on p values
model_21 = glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                  xManager + `xManufacturing Director` + 
                 xSingle + 
                 xESMedium + xESHigh + `xESVery High` + xJSMedium + 
                 xJSHigh + `xJSVery High` + 
                 `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_21)

#
#create next model after removing xJSMedium based on p values
model_22 = glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xManager + `xManufacturing Director` + 
                 xSingle + 
                 xESMedium + xESHigh + `xESVery High` +  
                 xJSHigh + `xJSVery High` + 
                 `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_22)

#
#create next model after removing xJSHigh based on p values
model_23 = glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                 xManager + `xManufacturing Director` + 
                 xSingle + 
                 xESMedium + xESHigh + `xESVery High` +  
                  `xJSVery High` + 
                 `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_23)

#create next model after removing xManager based on p values
model_24 = glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + avg_work_hour + xTravel_Frequently + 
                  `xManufacturing Director` + 
                 xSingle + 
                 xESMedium + xESHigh + `xESVery High` +  
                 `xJSVery High` + 
                 `xAgeGT35&LE45`, family = "binomial", 
               data = train)  

summary(model_24)

########################################################################
# With 14 significant variables in the model

final_model<- model_24
#based on final model co-efficients most impact vars both positive and negative
#xSingle - 0.90146
#xTravel_Frequently         0.81287
#avg_work_hour              0.66787
#xJSVery High`            -0.83771
#xAgeGT35&LE45`           -0.76763
#xManufacturing Director` -0.83923
#xESHigh                   -0.78402

#######################################################################
 

### Model Evaluation

#predicted probabilities of Attrition for train data to select optimum threshold

train_pred = predict(final_model, type = "response", 
                     newdata = train)


##########################################################################################
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=1000)

for (cutoff in cutoffs){
  predicted=as.numeric(train_pred>cutoff)
  
  TP=sum(predicted==1 & train$Attrition==1)
  FP=sum(predicted==1 & train$Attrition==0)
  FN=sum(predicted==0 & train$Attrition==1)
  TN=sum(predicted==0 & train$Attrition==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}
# lets remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

## ------------------------------------------------------------------------
#calculating all the measures
cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP,Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2)) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N))

#visulaizing all the measures

cutoff_viz=cutoff_data %>%
  dplyr::select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M) 

ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()

#We'll visualise lift separately because of its scale

cutoff_viz %>%
  filter(Criterion=="Lift") %>%
  ggplot(aes(x=cutoff,y=Value,color=Criterion))+geom_line()


####################################################################################################
########################################################################################################
#Cutoff with max KS
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff
table(test$Attrition,as.numeric(test_pred>KS_cutoff))

#Cutoff with max Accuracy
Acc_cutoff=cutoff_data$cutoff[which(cutoff_data$Accuracy==max(cutoff_data$Accuracy))][1]
Acc_cutoff
table(test$Attrition,as.numeric(test_pred>Acc_cutoff))


# Let's choose a cutoff value 0.2042042 ie ks as accuracy is not helping much to classify senseb=ntivity and specificity 
# properly for final model


#predicting on test
test_pred = predict(final_model, type = "response", 
                    newdata = test)

test_actual_attrition=  factor(ifelse(test$Attrition==1, "Yes", "No"))
test_cutoff_attrition<- factor(ifelse(test_pred >KS_cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

conf_final
acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.4786 close to 50 which is good as per KS statistics and acceptable

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
attrition_decile

#xSingle - singles tend to switch more, so company can promote policies for singles
#to retain them.
#xTravel_Frequently- people who travel more frequently have higher probability of 
#leaving the company. So company can put the rotation policies in place
#avg_work_hour- people who have more avg work hour tend to leave.
#company/management can look for option to reduce the work hours of such employees
#xJSVery High` -the employees who have very high job satisfaction level 
#stay with the company for longer. so HR and managers can looks to improve the
#job statisfaction of the employees
#xAgeGT35&LE45` - people more than 35 years of age generally stay with the company
#attrition rate for such employees is less
#Manufacturing Director- stay with the company longer
#xESHigh -if the work environment is highly satifies, employees don't leave the company
#so the company can look out for options to improve the work environment
