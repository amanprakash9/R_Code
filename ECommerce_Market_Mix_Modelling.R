#setwd() Set your working directory here

##################Problem Solving Approach##############################################
#Business Understanding
#Data Understanding
#Data Preparation, Deriving Metrics & EDA
#Modelling & Presentation of Results
################################################################

#####Business Understanding and AIM#####################################
# ElecKart is an e-commerce firm specialising in electronic products.
# Over the last one year, they had spent a significant amount of money in marketing.
# Occasionally, they had also offered big-ticket promotions (similar to the Big Billion Day).
# They are about to create a marketing budget for the next year which includes spending
# on commercials, online campaigns, and pricing & promotion strategies.
# The CFO feels that the money spent over last 12 months on marketing was not sufficiently
# impactful, and, that they can either cut on the budget or reallocate
# it optimally across marketing levers to improve the revenue response.

#AIM
# In this regard, we need to develop a market mix model to observe the actual impact 
# of different marketing variables over the last year.
# Alos Using our understanding of the model, we have to recommend the optimal budget 
# allocation for different marketing levers for the next year.

#################Data Understanding, Cleaning & Preparation#####################################

#Install and Load the required packages
install.packages("dplyr")
install.packages("lubridate")
install.packages("readxl")
install.packages("zoo")
install.packages("ggplot")
install.packages("MASS")
install.packages("car")
install.packages("caret")
install.packages("MLmetrics")

library(dplyr)
library(lubridate)
library(readxl)
library(zoo)
library(ggplot2)
library(MASS)
library(car)
library(caret)
library(MLmetrics)

options(scipen=999) #to disable scientific notation

#Load the data
ConsumerElectronics_data <- read.csv('ConsumerElectronics.csv',stringsAsFactors = F,na.strings=c("", "NA")) #to set all blanks to NA
View(ConsumerElectronics_data)
str(ConsumerElectronics_data) #1648824 obs. of  20 variables

#Treating Missing values
sum(is.na(ConsumerElectronics_data)) # there are 14712 NA values in the dataset
ConsumerElectronics_data<-na.omit(ConsumerElectronics_data) #ignoring the missing values is the best option as their proportion is way too less

#converting order id, order item id,cust_id, pincode to char class
ConsumerElectronics_data[,c(5,6,13,14)] <- sapply(ConsumerElectronics_data[,c(5,6,13,14)],as.character)

#duplicates by considering Order Item ID as this should be unique for every product order
sum(duplicated(ConsumerElectronics_data[,c(6)])) #166801 duplicate records are availble
ConsumerElectronics_data=ConsumerElectronics_data[!duplicated(ConsumerElectronics_data[,c(6)]),] #removing duplicates

#removing records out of July 2015 & June 2016
ConsumerElectronics_data$order_date<-as.Date(ConsumerElectronics_data$order_date)
summary(ConsumerElectronics_data$order_date) #there are values lying out of required range
Date_Start<-as.Date("2015-07-01")  #starting July 2015
Date_End<-as.Date("2016-06-30") #Ending June 2016
ConsumerElectronics_data<-filter(ConsumerElectronics_data,ConsumerElectronics_data$order_date>=Date_Start & ConsumerElectronics_data$order_date<=Date_End)
summary(ConsumerElectronics_data$order_date)
summary(ConsumerElectronics_data$Year) #no records out of range 2015 to 2016
summary(ConsumerElectronics_data$Month) #no records out of range 1 to 12

#deliverybdays and deliverycdays are holding '\N' and negative values
ConsumerElectronics_data$deliverycdays[ConsumerElectronics_data$deliverycdays == "\\N" | ConsumerElectronics_data$deliverycdays < 0] <- 0
ConsumerElectronics_data$deliverybdays[ConsumerElectronics_data$deliverybdays == "\\N" | ConsumerElectronics_data$deliverybdays < 0] <- 0
ConsumerElectronics_data$deliverycdays<-as.numeric(ConsumerElectronics_data$deliverycdays)
ConsumerElectronics_data$deliverybdays<-as.numeric(ConsumerElectronics_data$deliverybdays)

#checking outliers on relevant columns
length(boxplot.stats(ConsumerElectronics_data$gmv)$out) #145710
length(boxplot.stats(ConsumerElectronics_data$units)$out) #20146
length(boxplot.stats(ConsumerElectronics_data$deliverybdays)$out) #320366
length(boxplot.stats(ConsumerElectronics_data$deliverycdays)$out) #322132
length(boxplot.stats(ConsumerElectronics_data$sla)$out) #39675
length(boxplot.stats(ConsumerElectronics_data$product_mrp)$out) #144615
#We are not treating outliers as these are not too low or too high in number

#negative values in cust id and pincode but not treating these as they aren't relevant for the study

#removing records where revenue is more than MRP multuplied by order units
ConsumerElectronics_data <- subset(ConsumerElectronics_data, (product_mrp*units) >= gmv)
str(ConsumerElectronics_data) #1443630 obs. of  20 variables

#removing records where sla is negative
sum(ConsumerElectronics_data$product_procurement_sla <0) #61303 - ~4% of the records
ConsumerElectronics_data <- subset(ConsumerElectronics_data, product_procurement_sla >= 0)

#generating week numbers as we have to recommend investments on weekly basis
ConsumerElectronics_data$week <- isoweek(ConsumerElectronics_data$order_date)
summary(ConsumerElectronics_data$week) 

sum(is.na(ConsumerElectronics_data)) #no missing values
####################################################################################################
#Merging files

###########reading media investment sheet
Media_Investment <- read_excel("Media data and other information.xlsx",sheet = 2)
View(Media_Investment)
str(Media_Investment) #13 obs. of  12 variables

#assigning appropriate column names
names(Media_Investment) <- NULL 
colnames(Media_Investment) = Media_Investment[1, ]
Media_Investment = Media_Investment[-1, ]

#Adding a year month column and generating # of days in that month
Media_Investment$YearMon<-as.yearmon(paste(Media_Investment$Month,Media_Investment$Year),"%m %Y")
Media_Investment$DaysInMonth<-days_in_month(Media_Investment$YearMon)

#deriving amount invested per day of month - helps in derving investments on weekly basis later
Media_Investment[,c(-1,-2,-13,-14)] <- sapply(Media_Investment[,c(-1,-2,-13,-14)],as.numeric)
Media_Investment[,c(-1,-2,-13,-14)]<-Media_Investment[,c(-1,-2,-13,-14)]/Media_Investment$DaysInMonth

#Replacing NAs with 0
Media_Investment$Radio[is.na(Media_Investment$Radio)] <- 0
Media_Investment$Other[is.na(Media_Investment$Other)] <- 0

View(Media_Investment)
str(Media_Investment) #12 obs. of  14 variables

#merging consumer electronics with media investment by Month
ConsumerElectronics_Final <- merge(ConsumerElectronics_data, Media_Investment[,c(-1,-13,-14)], by = 'Month', all.x = TRUE)
View(ConsumerElectronics_Final)
str(ConsumerElectronics_Final) #1382327 obs. of  31 variables

#####reading product list sheet 
Product_list <- read_excel("Media data and other information.xlsx",sheet = 1)
View(Product_list)
str(Product_list) #75 obs. of  3 variables
sum(is.na(Product_list)) #no NA or missing values
colnames(Product_list)[1] <- "product_analytic_vertical"

#checking unique product categories in Product_list
unique(Product_list$product_analytic_vertical) #74 unique ignoring Total

#checking unique product categories in ConsumerElectronics_Final
unique(ConsumerElectronics_Final$product_analytic_vertical) #72 unique - 2 were removed because of revenue > mrp
#\\N available in both consumer electronics and product category sheets

#merging consumer electronics with product list by Vertical
ConsumerElectronics_Final <- merge(ConsumerElectronics_Final, Product_list, by = 'product_analytic_vertical')
View(ConsumerElectronics_Final)
str(ConsumerElectronics_Final) #1382327 obs. of  33 variables

######reading nps score  
nps_score <- read_excel("Media data and other information.xlsx",sheet = 4)
nps_score<-as.data.frame(nps_score)
nps_score[,1]<-NULL
names(nps_score) <- NULL
nps_score<-as.data.frame(t(nps_score))
nps_score$Month <- c(7:12,1:6)
colnames(nps_score)[1] <- "NPS_Score"

#merging consumer electronics with nps data by Month
ConsumerElectronics_Final <- merge(ConsumerElectronics_Final, nps_score, by = 'Month')
View(ConsumerElectronics_Final)
str(ConsumerElectronics_Final) #1382327 obs. of  34 variables

######reading special sales data
spcl_sales <- read_excel("Media data and other information.xlsx",sheet = 3)
str(spcl_sales) #12 obs. of  2 variables
View(spcl_sales)
#creating a data frame with all the dates available in the sheet
spcl_sales_dates <- setNames(data.frame(matrix(ncol = 3, nrow = 44)), c("SpecialDates", "week","special_sales"))
spcl_sales_dates$SpecialDates <- c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28",
                "2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17","2015-11-07",
                "2015-11-08","2015-11-09","2015-11-10","2015-10-11","2015-10-12","2015-11-13",
                "2015-11-14","2015-12-25","2015-12-26","2015-12-27","2015-12-28","2015-12-29",
                "2015-12-30","2015-12-31","2016-01-01","2016-01-02","2016-01-03","2016-01-20",
                "2016-01-21","2016-01-22","2016-02-01","2016-02-02","2016-02-20","2016-02-21",
                "2016-02-14","2016-02-15","2016-03-07","2016-03-08","2016-03-09","2016-05-25",
                "2016-05-26","2016-05-27")
spcl_sales_dates$week <- isoweek(spcl_sales_dates$SpecialDates)
spcl_sales_dates$special_sales <- 1 # "1" indicates the week has special sales (derived metric)
spcl_sales_dates$SpecialDates <- NULL #column not required
spcl_sales_dates=spcl_sales_dates[!duplicated(spcl_sales_dates[,c(1)]),] #removing duplicates

#merging the spcl_sales data to ConsumerElectronics_Final data frame by week
ConsumerElectronics_Final <- merge(ConsumerElectronics_Final,spcl_sales_dates,by="week",all.x=TRUE)
ConsumerElectronics_Final$special_sales[is.na(ConsumerElectronics_Final$special_sales)] <- 0  #"0" indicates it's non-special sales
View(ConsumerElectronics_Final)
str(ConsumerElectronics_Final) #1382327 obs. of  35 variables

#removing spaces for columns
names(ConsumerElectronics_Final)[names(ConsumerElectronics_Final) == "Total Investment"] <- "TotalInvestment"
names(ConsumerElectronics_Final)[names(ConsumerElectronics_Final) == "Content Marketing"] <- "ContentMarketing"
names(ConsumerElectronics_Final)[names(ConsumerElectronics_Final) == "Online marketing"] <- "OnlineMarketing"

View(ConsumerElectronics_Final)
str(ConsumerElectronics_Final) #1382327 obs. of  35 variables

sum(is.na(ConsumerElectronics_Final)) #no missing values
########################################################################################################

##Derving Metrics

#As part of data merging, we derived festival week (special_sales flag). 

#deriving weekly investments by aggregating on unique order dates
tempdf<-ConsumerElectronics_Final[!duplicated(ConsumerElectronics_Final$order_date), ] 
tempvar<-c("TotalInvestment","TV","Digital","Sponsorship","ContentMarketing","OnlineMarketing","Affiliates","SEM","Radio","Other")
for (myVar in tempvar) {
  weekly_Inv<-tempdf %>% group_by(week = isoweek(order_date)) %>% summarise(myVar = sum(!!sym(myVar)))
  colnames(weekly_Inv)[2] <- myVar
  ConsumerElectronics_Final[,myVar]<-NULL
  ConsumerElectronics_Final <- merge(ConsumerElectronics_Final,weekly_Inv,by="week",all.x=TRUE)
}

#Deriving discount% based on GMV and MRP columns
ConsumerElectronics_Final$order_mrp<-ConsumerElectronics_Final$product_mrp*ConsumerElectronics_Final$units
ConsumerElectronics_Final$order_discount <- ConsumerElectronics_Final$order_mrp-ConsumerElectronics_Final$gmv
ConsumerElectronics_Final$discountpercentage <- 100 * +(ConsumerElectronics_Final$order_discount/ConsumerElectronics_Final$order_mrp)
summary(ConsumerElectronics_Final$discountpercentage) #0 NAs
ConsumerElectronics_Final$order_mrp<-NULL
ConsumerElectronics_Final$order_discount <- NULL

#weekly units
weekly_units <- aggregate(units~week, ConsumerElectronics_Final, sum, na.rm=TRUE)
colnames(weekly_units)[2] <- "weekly_sold_units"
ConsumerElectronics_Final<- merge(ConsumerElectronics_Final,weekly_units,by="week",all.x=TRUE)

#deriving weekly gmv/revenue and also Adding High and low revenue flags based on mean weekly gmv
weekly_gmv <- aggregate(gmv~week, ConsumerElectronics_Final, sum, na.rm=TRUE)
weekly_gmv$highrevenue  <- ifelse(weekly_gmv$gmv >= mean(weekly_gmv$gmv),1,0)
colnames(weekly_gmv)[2] <- "weekly_gmv"
ConsumerElectronics_Final<- merge(ConsumerElectronics_Final,weekly_gmv,by="week",all.x=TRUE)

#Adding High and low investment flags based on mean weekly investment
ConsumerElectronics_Final$highinvestment  <- ifelse(ConsumerElectronics_Final$TotalInvestment >= mean(ConsumerElectronics_Final$TotalInvestment),1,0)

#converting weekly gmv into crores as the investment values are in crores
ConsumerElectronics_Final$weekly_gmv<-ConsumerElectronics_Final$weekly_gmv/10000000

#calculating weekly average discount
weekly_avg_discount <- aggregate(discountpercentage~week, ConsumerElectronics_Final, mean, na.rm=TRUE)
colnames(weekly_avg_discount)[2] <- "weekly_avg_discount"
ConsumerElectronics_Final<- merge(ConsumerElectronics_Final,weekly_avg_discount,by="week",all.x=TRUE)
summary(ConsumerElectronics_Final$discountpercentage)

#changing week sequence - July 2015 to June 2016 as 1 to 53 - numbering changes after "2016-01-03" 
summary(ConsumerElectronics_Final$week)
ConsumerElectronics_Final$week <- ifelse(ConsumerElectronics_Final$order_date <= as.Date("2016-01-03"),ConsumerElectronics_Final$week-26,ConsumerElectronics_Final$week+27)

View(ConsumerElectronics_Final)
str(ConsumerElectronics_Final) #1382327 obs. of  41 variables

sum(is.na(ConsumerElectronics_Final)) #no missing values
#################################################################################################################

#Exploratory Data Analysis (EDA)

#considering derived metrics and other variables of interest
eda_dataframe<-ConsumerElectronics_Final[,c(1,9,14,25:41)]
View(eda_dataframe) #Base file

#####EDA on the base file
#reusable functions for line and point graphs
plot_geomline <- function(datafram, aesY, aesYcolour) {
  ggplot(data = datafram, aes(x = week)) + 
    geom_line(aes(y = weekly_gmv, colour = "weekly_gmv")) +
    geom_line(aes(y = aesY, colour = aesYcolour)) +
    xlab('week') +
    ylab('') +
    scale_colour_manual("", values=c("red","blue"))
}

plot_geompoint <- function(datafram, aesY1, aesY1colour, aesY2, aesY2colour) {
  ggplot(data = datafram, aes(x = week)) + 
    geom_point(aes(y = aesY1, colour = aesY1colour,shape=aesY1colour,size=4)) +
    geom_point(aes(y = aesY2, colour = aesY2colour,shape=aesY2colour,size=4)) +
    xlab('week') +
    ylab('') +
    scale_shape_manual("",values=c(16, 17))+
    scale_colour_manual("", values=c("red","blue"))
}

#1. Plot showing investments and revenue on a weekly basis
plot_geomline(eda_dataframe, eda_dataframe$TotalInvestment, "TotalInvestment")
cor(eda_dataframe$TotalInvestment,eda_dataframe$weekly_gmv) #0.58
# This shows investment and revenue are directly proportional for few weeks

#2. Let's see how TV investment is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$TV, "TV")
cor(eda_dataframe$TV,eda_dataframe$weekly_gmv) #0.29
#looks like TV investment and revenue are not that related

#3. Let's see how Digital investment is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$Digital, "Digital")
cor(eda_dataframe$Digital,eda_dataframe$weekly_gmv) #0.59
#looks like Digital investment and revenue are related

#4. Let's see how Sponsorship is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$Sponsorship, "Sponsorship")
cor(eda_dataframe$Sponsorship,eda_dataframe$weekly_gmv) #0.48
#looks like sponsorship and revenue are related for some weeks

#5. Let's see how Content Marketing is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$ContentMarketing, "ContentMarketing")
cor(eda_dataframe$ContentMarketing,eda_dataframe$weekly_gmv) #0.61
#looks like Content Marketing and revenue are related

#6. Let's see how Online Marketing is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$OnlineMarketing, "OnlineMarketing")
cor(eda_dataframe$OnlineMarketing,eda_dataframe$weekly_gmv) #0.39
#looks like Online Marketing and revenue are directly related except for few weeks

#7. Let's see how Affiliates is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$Affiliates, "Affiliates") 
cor(eda_dataframe$Affiliates,eda_dataframe$weekly_gmv) #0.32
#looks like Affiliates and revenue are not related

#8. Let's see how SEM is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$SEM, "SEM")
cor(eda_dataframe$SEM,eda_dataframe$weekly_gmv) #0.61
#looks like SEM and revenue are related

#9. Let's see how Radio is related to revenue
plot_geomline(eda_dataframe, eda_dataframe$Radio, "Radio")
#Radio and revenue are not related & also there are missing values for most of the weeks

#10. Let's see how Other channels are related to revenue
plot_geomline(eda_dataframe, eda_dataframe$Other, "Other")
#Other channels and revenue are not related & also there are missing values for most of the weeks

#11. Let's see how Special Sales and High Revenue are related
plot_geompoint(eda_dataframe, eda_dataframe$special_sales, "special_sales", eda_dataframe$highrevenue, "highrevenue") #mostly related
#34 out of 53 weeks overlapped

#12. Let's see how High Revenue and High Investment are related
plot_geompoint(eda_dataframe, eda_dataframe$highinvestment, "highinvestment", eda_dataframe$highrevenue, "highrevenue") #mostly related
#28 out of 53 weeks overlapped

#13. How no. of units sold is related to gmv
cor(eda_dataframe$weekly_sold_units,eda_dataframe$weekly_gmv)
#0.97 - Highly related

#14. How weekly average discount is related to gmv
plot_geomline(eda_dataframe, eda_dataframe$weekly_avg_discount, "weekly_avg_discount")
cor(eda_dataframe$weekly_avg_discount,eda_dataframe$weekly_gmv)
#0.65 strongly related

#15. Plotting special sales and revenues together
ggplot(eda_dataframe, aes(x=week, y=weekly_gmv)) +
  geom_point(aes(shape=as.factor(special_sales), color=as.factor(special_sales), size=4))+
  geom_line()
#not every special sale moved up the revenues

#15. Plotting sla and revenues together
cor(ConsumerElectronics_Final$sla,ConsumerElectronics_Final$gmv)
#-0.14 - negatively related - so as delivery time increases revenue decreases 

## Creating subsets of dataframes for three product sub-categories 
##- camera accessory, home audio and gaming accessory
Electronics_CameraAccessory <- subset(ConsumerElectronics_Final, product_analytic_sub_category == "CameraAccessory")
str(Electronics_CameraAccessory) #200069 obs. of  41 variables
Electronics_HomeAudio <- subset(ConsumerElectronics_Final, product_analytic_sub_category == "HomeAudio")
str(Electronics_HomeAudio) #103198 obs. of  41 variables
Electronics_GamingAccessory <- subset(ConsumerElectronics_Final, product_analytic_sub_category == "GamingAccessory")
str(Electronics_GamingAccessory) #175478 obs. of  41 variables

####-------EDA for Camera Accessory -------####
#considering variables of interest
edaCameraAccessoryDataFrame<-Electronics_CameraAccessory[,c(1,9,10,20,25:41)]
str(edaCameraAccessoryDataFrame) #200069 obs. of  21 variables
View(edaCameraAccessoryDataFrame)

# 1. Plot showing total investments and revenue on a weekly basis
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$TotalInvestment, "TotalInvestment") 
cor(edaCameraAccessoryDataFrame$TotalInvestment,edaCameraAccessoryDataFrame$weekly_gmv)#0.47
# Plot shows investment and revenue are directly proportional for few weeks

#2. Let's see how TV investment is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$TV, "TV") 
cor(edaCameraAccessoryDataFrame$TV,edaCameraAccessoryDataFrame$weekly_gmv) #0.24
#plot shows positive relation between TV Inv and Revenue but not highly related

#3. Let's see how Digital investment is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$Digital, "Digital") 
cor(edaCameraAccessoryDataFrame$Digital,edaCameraAccessoryDataFrame$weekly_gmv) #0.47
#Digital investment and revenue are related

#4. Let's see how Sponsorship is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$Sponsorship, "Sponsorship")
cor(edaCameraAccessoryDataFrame$Sponsorship,edaCameraAccessoryDataFrame$weekly_gmv) #0.36
#Sponsorship is not in line with revenue

#5. Let's see how Content Marketing is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$ContentMarketing, "ContentMarketing") 
cor(edaCameraAccessoryDataFrame$ContentMarketing,edaCameraAccessoryDataFrame$weekly_gmv) #0.50
#Except for weeks ~15-20, content marketing line lies almost flat for all other weeks

#6. Let's see how Online Marketing is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$OnlineMarketing, "OnlineMarketing") 
cor(edaCameraAccessoryDataFrame$OnlineMarketing,edaCameraAccessoryDataFrame$weekly_gmv) #0.36
#online marketing is not strongly related to revenue

#7. Let's see how Affiliates is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$Affiliates, "Affiliates") 
cor(edaCameraAccessoryDataFrame$Affiliates,edaCameraAccessoryDataFrame$weekly_gmv) #0.31
#Affiliate marketing is again not mostly related to revenue

#8. Let's see how SEM is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$SEM, "SEM") 
cor(edaCameraAccessoryDataFrame$SEM,edaCameraAccessoryDataFrame$weekly_gmv) #0.50
#Except for few weeks there is strong relation here

#9. Let's see how Radio is related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$Radio, "Radio") 
#Radio and revenue are not related & also there are missing values for most of the weeks

#10. Let's see how Other channels are related to revenue
plot_geomline(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$Other, "Other") 
#Other channels and revenue are not related & also there are missing values for most of the weeks

#11. Let's see how Special Sales and High Revenue are related
plot_geompoint(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$special_sales, "special_sales", edaCameraAccessoryDataFrame$highrevenue, "highrevenue") #mostly related
#36 out of 53 weeks overlapped

#12. Let's see how High Revenue and High Investment are related
plot_geompoint(edaCameraAccessoryDataFrame, edaCameraAccessoryDataFrame$highinvestment, "highinvestment", edaCameraAccessoryDataFrame$highrevenue, "highrevenue") #mostly related
#28 out of 53 weeks overlapped

#13. How no. of units sold is related to gmv
cor(edaCameraAccessoryDataFrame$weekly_sold_units,edaCameraAccessoryDataFrame$weekly_gmv)
#0.96 Highly related

#14. How MRP is related to units sold
cor(edaCameraAccessoryDataFrame$units ,edaCameraAccessoryDataFrame$product_mrp)
#-0.02 not related - increase in price leading to decrease in orders

#15. How MRP is related to revenue
cor(edaCameraAccessoryDataFrame$gmv,edaCameraAccessoryDataFrame$product_mrp)
# 0.93 - Highly related 

#16. How discount is related to units sold
cor(edaCameraAccessoryDataFrame$weekly_sold_units,edaCameraAccessoryDataFrame$weekly_avg_discount)
# 0.62 - sales are strongly related to discounts

#17. How weekly revenue is related to weekly average discount
cor(edaCameraAccessoryDataFrame$weekly_gmv,edaCameraAccessoryDataFrame$weekly_avg_discount)
# 0.62 - reveune is strongly related to discounts

####-------EDA for Electronics HomeAudio -------####
#considering variables of interest
edaElectronicsHomeAudioDataFrame<-Electronics_HomeAudio[,c(1,9,10,20,25:41)]
View(edaElectronicsHomeAudioDataFrame)
str(edaElectronicsHomeAudioDataFrame) #103198 obs. of  21 variables

# 1. Plot showing total investments and revenue on a weekly basis
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$TotalInvestment, "TotalInvestment") 
cor(edaElectronicsHomeAudioDataFrame$TotalInvestment,edaElectronicsHomeAudioDataFrame$weekly_gmv)#0.54
# Plot shows investment and revenue are directly proportional for few weeks

#2. Let's see how TV investment is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$TV, "TV") 
cor(edaElectronicsHomeAudioDataFrame$TV,edaElectronicsHomeAudioDataFrame$weekly_gmv) #0.32
#plot shows positive relation between TV Inv and Revenue but not highly related

#3. Let's see how Digital investment is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$Digital, "Digital") 
cor(edaElectronicsHomeAudioDataFrame$Digital,edaElectronicsHomeAudioDataFrame$weekly_gmv) #0.55
#Except for weeks ~15-20, the two variables are related

#4. Let's see how Sponsorship is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$Sponsorship, "Sponsorship")
cor(edaElectronicsHomeAudioDataFrame$Sponsorship,edaElectronicsHomeAudioDataFrame$weekly_gmv) #0.44
#Sponsorship is in line with revenue

#5. Let's see how Content Marketing is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$ContentMarketing, "ContentMarketing") 
cor(edaElectronicsHomeAudioDataFrame$ContentMarketing,edaElectronicsHomeAudioDataFrame$weekly_gmv) #0.57
# content marketing line lies almost flat for all weeks

#6. Let's see how Online Marketing is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$OnlineMarketing, "OnlineMarketing") 
cor(edaElectronicsHomeAudioDataFrame$OnlineMarketing,edaElectronicsHomeAudioDataFrame$weekly_gmv) #0.38
#online marketing is not related to revenue

#7. Let's see how Affiliates is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$Affiliates, "Affiliates") 
cor(edaElectronicsHomeAudioDataFrame$Affiliates,edaElectronicsHomeAudioDataFrame$weekly_gmv) #0.32
#Affiliate marketing not that related to revenue

#8. Let's see how SEM is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$SEM, "SEM") 
cor(edaElectronicsHomeAudioDataFrame$SEM,edaElectronicsHomeAudioDataFrame$weekly_gmv) #0.57
#Except for few weeks there is relation here

#9. Let's see how Radio is related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$Radio, "Radio") 
#Radio and revenue are not related & also there are missing values for most of the weeks

#10. Let's see how Other channels are related to revenue
plot_geomline(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$Other, "Other") 
#Other channels and revenue are not related & also there are missing values for most of the weeks

#11. Let's see how Special Sales and High Revenue are related
plot_geompoint(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$special_sales, "special_sales", edaElectronicsHomeAudioDataFrame$highrevenue, "highrevenue") #mostly related
#37 out of 53 weeks overlapped

#12. Let's see how High Revenue and High Investment are related
plot_geompoint(edaElectronicsHomeAudioDataFrame, edaElectronicsHomeAudioDataFrame$highinvestment, "highinvestment", edaElectronicsHomeAudioDataFrame$highrevenue, "highrevenue") #mostly related
#28 out of 53 weeks overlapped

#13. How no. of units sold is related to gmv
cor(edaElectronicsHomeAudioDataFrame$weekly_sold_units,edaElectronicsHomeAudioDataFrame$weekly_gmv)
#0.97 Highly related

#14. How MRP is related to units sold
cor(edaElectronicsHomeAudioDataFrame$units ,edaElectronicsHomeAudioDataFrame$product_mrp)
#-0.02 not related - increase in price leading to decrease in orders

#15. How MRP is related to revenue
cor(edaElectronicsHomeAudioDataFrame$gmv,edaElectronicsHomeAudioDataFrame$product_mrp)
# 0.80 - Highly related 

#16. How discount is related to units sold
cor(edaElectronicsHomeAudioDataFrame$weekly_sold_units,edaElectronicsHomeAudioDataFrame$weekly_avg_discount)
# 0.64 - sales are related to discounts

#17. How weekly revenue is related to weekly average discount
cor(edaElectronicsHomeAudioDataFrame$weekly_gmv,edaElectronicsHomeAudioDataFrame$weekly_avg_discount)
# 0.65 - revenue is related to discounts

####-------EDA for Electronics Gaming Accessory -------####
#considering variables of interest
edaElectronicsGamingAccessoryDataFrame<-Electronics_GamingAccessory[,c(1,9,10,20,25:41)]
View(edaElectronicsGamingAccessoryDataFrame)
str(edaElectronicsGamingAccessoryDataFrame)#175478 obs. of  21 variables

# 1. Plot showing total investments and revenue on a weekly basis
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$TotalInvestment, "TotalInvestment") 
cor(edaElectronicsGamingAccessoryDataFrame$TotalInvestment,edaElectronicsGamingAccessoryDataFrame$weekly_gmv)#0.55
# Plot shows investment and revenue are directly proportional

#2. Let's see how TV investment is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$TV, "TV") 
cor(edaElectronicsGamingAccessoryDataFrame$TV,edaElectronicsGamingAccessoryDataFrame$weekly_gmv) #0.28
#plot shows positive relation between TV Inv and Revenue but not related

#3. Let's see how Digital investment is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$Digital, "Digital") 
cor(edaElectronicsGamingAccessoryDataFrame$Digital,edaElectronicsGamingAccessoryDataFrame$weekly_gmv) #0.56
#Except for weeks ~15-20, the two variables are related

#4. Let's see how Sponsorship is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$Sponsorship, "Sponsorship")
cor(edaElectronicsGamingAccessoryDataFrame$Sponsorship,edaElectronicsGamingAccessoryDataFrame$weekly_gmv) #0.44
#Sponsorship is in line with revenue

#5. Let's see how Content Marketing is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$ContentMarketing, "ContentMarketing") 
cor(edaElectronicsGamingAccessoryDataFrame$ContentMarketing,edaElectronicsGamingAccessoryDataFrame$weekly_gmv) #0.58
# content marketing line lies almost flat for all weeks

#6. Let's see how Online Marketing is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$OnlineMarketing, "OnlineMarketing") 
cor(edaElectronicsGamingAccessoryDataFrame$OnlineMarketing,edaElectronicsGamingAccessoryDataFrame$weekly_gmv) #0.37
#online marketing is not related to revenue

#7. Let's see how Affiliates is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$Affiliates, "Affiliates") 
cor(edaElectronicsGamingAccessoryDataFrame$Affiliates,edaElectronicsGamingAccessoryDataFrame$weekly_gmv) #0.31
#Affiliate marketing not related to revenue

#8. Let's see how SEM is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$SEM, "SEM") 
cor(edaElectronicsGamingAccessoryDataFrame$SEM,edaElectronicsGamingAccessoryDataFrame$weekly_gmv) #0.58
#Except for few weeks there is relation here

#9. Let's see how Radio is related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$Radio, "Radio") 
#Radio and revenue are not related & also there are missing values for most of the weeks

#10. Let's see how Other channels are related to revenue
plot_geomline(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$Other, "Other") 
#Other channels and revenue are not related & also there are missing values for most of the weeks

#11. Let's see how Special Sales and High Revenue are related
plot_geompoint(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$special_sales, "special_sales", edaElectronicsGamingAccessoryDataFrame$highrevenue, "highrevenue") #mostly related
#34 out of 53 weeks overlapped

#12. Let's see how High Revenue and High Investment are related
plot_geompoint(edaElectronicsGamingAccessoryDataFrame, edaElectronicsGamingAccessoryDataFrame$highinvestment, "highinvestment", edaElectronicsGamingAccessoryDataFrame$highrevenue, "highrevenue") #mostly related
#28 out of 53 weeks overlapped

#13. How no. of units sold is related to gmv
cor(edaElectronicsGamingAccessoryDataFrame$weekly_sold_units,edaElectronicsGamingAccessoryDataFrame$weekly_gmv)
#0.96 Highly related

#14. How MRP is related to units sold
cor(edaElectronicsGamingAccessoryDataFrame$units ,edaElectronicsGamingAccessoryDataFrame$product_mrp)
#-0.01 negatively related - increase in price leading to decrease in orders

#15. How MRP is related to revenue
cor(edaElectronicsGamingAccessoryDataFrame$gmv,edaElectronicsGamingAccessoryDataFrame$product_mrp)
# 0.27 - Not related 

#16. How discount is related to units sold
cor(edaElectronicsGamingAccessoryDataFrame$weekly_sold_units,edaElectronicsGamingAccessoryDataFrame$weekly_avg_discount)
# 0.63 - sales are related to discounts

#17. How weekly revenue is related to weekly average discount
cor(edaElectronicsGamingAccessoryDataFrame$weekly_gmv,edaElectronicsGamingAccessoryDataFrame$weekly_avg_discount)
# 0.63 - revenue is related to discounts

## Please check the presentation for the KPIs  derived from the above analysis ############

##############################Modeling##################################################################

########Camera Accessory################################################################################

####simple linear regression#####################
str(Electronics_CameraAccessory) #200069 obs. of  41 variables
CameraAccessory_Model<-Electronics_CameraAccessory[,-c(1,4,5,7,8,15:19)] #as these are not relevant to our study
str(CameraAccessory_Model)  #200069 obs. of  31 variables

#Creating dummy variables
#product_analytic_vertical
dummy_vertical <-data.frame(model.matrix(~product_analytic_vertical, data=CameraAccessory_Model))
dummy_vertical <- dummy_vertical[,-1]  

#creating levels
# order_payment_type
CameraAccessory_Model$s1_fact.order_payment_type<-as.factor(CameraAccessory_Model$s1_fact.order_payment_type)
levels(CameraAccessory_Model$s1_fact.order_payment_type)<-c(1,0)
# Assigning 1 if the payment type is COD and 0 if Prepaid
CameraAccessory_Model$s1_fact.order_payment_type<- as.numeric(levels(CameraAccessory_Model$s1_fact.order_payment_type))[CameraAccessory_Model$s1_fact.order_payment_type]

#combining dummy
CameraAccessory_Model_1 <- cbind(CameraAccessory_Model[,-2], dummy_vertical)

# View the new dataset 
View(CameraAccessory_Model_1)

#scaling
CameraAccessory_Model_1_scl<-scale(CameraAccessory_Model_1)
CameraAccessory_Model_1_scl<-as.data.frame(CameraAccessory_Model_1)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Model_1_scl), 0.7*nrow(CameraAccessory_Model_1_scl))

train_Cam_LR=CameraAccessory_Model_1_scl[indices,]
test_Cam_LR = CameraAccessory_Model_1_scl[-indices,]

# Develop the first model 
LR_model_1 <-lm(weekly_gmv~.,data=train_Cam_LR[,])
summary(LR_model_1)
# Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761

# Apply the stepwise approach
step <- stepAIC(LR_model_1, direction="both")

# Run the step object
step

#model 2
model_2 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraFilmRolls + product_analytic_verticalCameraTripod + 
               product_analytic_verticalFlashShoeAdapter + product_analytic_verticalLens, 
             data = train_Cam_LR[,])
summary(model_2) #Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761
sort(vif(model_2))

#removing FlashShoeAdapter with low significance
model_3 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraFilmRolls + product_analytic_verticalCameraTripod + 
               product_analytic_verticalLens, 
             data = train_Cam_LR[,])
summary(model_3) #Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761 
sort(vif(model_3))

#removing product_analytic_verticalCameraBag with high VIF and low significance
model_4 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraFilmRolls + product_analytic_verticalCameraTripod + 
               product_analytic_verticalLens, 
             data = train_Cam_LR[,])
summary(model_4) #Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761 
sort(vif(model_4))

#removing product_analytic_verticalLens with low significance
model_5 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraFilmRolls + product_analytic_verticalCameraTripod , 
             data = train_Cam_LR[,])
summary(model_5) #Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761  
sort(vif(model_5))

#removing product_analytic_verticalCameraFilmRolls with low significance
model_6 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraTripod , 
             data = train_Cam_LR[,])
summary(model_6) #Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761
sort(vif(model_6))

#removing discountpercentage with low significance
model_7 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraTripod , 
             data = train_Cam_LR[,])
summary(model_7) #Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761 
sort(vif(model_7))

#removing gmv with low significance
model_8 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraTripod , 
             data = train_Cam_LR[,])
summary(model_8) #Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9761
sort(vif(model_8))

#removing TotalInvestment with high VIF
model_9 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + product_mrp + 
               NPS_Score + special_sales + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
               product_analytic_verticalCameraTripod , 
             data = train_Cam_LR[,])
summary(model_9) #Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9759  
sort(vif(model_9))

#removing Month with low significance
model_10 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + product_mrp + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                SEM + Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_10) #Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9759 
sort(vif(model_10))

#removing SEM with high VIF
model_11 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + product_mrp + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_11) #Multiple R-squared:  0.9757,	Adjusted R-squared:  0.9757 
sort(vif(model_11))

#removing Affiliates with high VIF
model_12 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + product_mrp + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + 
                Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_12) #Multiple R-squared:  0.9757,	Adjusted R-squared:  0.9757
sort(vif(model_12))

#removing product_mrp with low significance
model_13 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + 
                Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_13) #Multiple R-squared:  0.9757,	Adjusted R-squared:  0.9757
sort(vif(model_13))

#removing ContentMarketing with high VIF
model_14 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + OnlineMarketing + 
                Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_14) #Multiple R-squared:  0.9746,	Adjusted R-squared:  0.9746
sort(vif(model_14))

#removing Other with high VIF
model_15 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_15) #Multiple R-squared:  0.9722,	Adjusted R-squared:  0.9722
sort(vif(model_15))

#removing deliverycdays with high VIF
model_16 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_16) #Multiple R-squared:  0.9722,	Adjusted R-squared:  0.9722
sort(vif(model_16))

#removing Sponsorship with high VIF
model_17 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                product_analytic_verticalCameraTripod , 
              data = train_Cam_LR[,])
summary(model_17) #Multiple R-squared:  0.9696,	Adjusted R-squared:  0.9696  
sort(vif(model_17))

#removing NPS_Score with high VIF
LR_Cam_model_18 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                       s1_fact.order_payment_type + 
                       special_sales + TV + Digital + 
                       OnlineMarketing + 
                       Radio + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + 
                       product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                       product_analytic_verticalCameraTripod , 
                     data = train_Cam_LR[,])
summary(LR_Cam_model_18) #Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9638  
sort(vif(LR_Cam_model_18))
#All these 15 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- LR_Cam_model_18
train_data <- train_Cam_LR
test_data <- test_Cam_LR
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2     RMSE       MAE      MAPE
#  0.963641 1.019679 0.7974991 0.1417514

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.121 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_fun <- function(var){
  tempcol<-as.numeric(train_data[,var])
  ela_coeff <-final_model$coefficients[var]*mean(tempcol)/mean(train_data$weekly_gmv)
  return(ela_coeff)
}
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

#########Multiplicative modelling - to see interactions between the KPIs#############

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Model_1), 0.7*nrow(CameraAccessory_Model_1))
train_cam_mult=CameraAccessory_Model_1[indices,]
test_cam_mult = CameraAccessory_Model_1[-indices,]

#to avoid NAs while taking log values
train_cam_mult[train_cam_mult == 0] <- 0.00001
test_cam_mult[test_cam_mult == 0] <- 0.00001

#log of values
train_cam_mult <- log(train_cam_mult)
test_cam_mult <- log(test_cam_mult)

sum(is.na(train_cam_mult))# no missing values

# Develop the first model 
model_1 <-lm(weekly_gmv~.,data=train_cam_mult[,])
summary(model_1)
#Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")

# Run the step object
step

#model 2
model_2 <-lm(formula = weekly_gmv ~ Month + Year + gmv + units + s1_fact.order_payment_type + 
               product_mrp + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraTripod + 
               product_analytic_verticalFlash + product_analytic_verticalFlashShoeAdapter, 
             data = train_cam_mult[,])
summary(model_2) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692
sort(vif(model_2))

#removing product_analytic_verticalFlash having relatively high VIF and low significance
model_3 <-lm(formula = weekly_gmv ~ Month + Year + gmv + units + s1_fact.order_payment_type + 
               product_mrp + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraTripod + 
               product_analytic_verticalFlashShoeAdapter, 
             data = train_cam_mult[,])
summary(model_3) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692
sort(vif(model_3))

#removing product_mrp having relatively high VIF and low significance
model_4 <-lm(formula = weekly_gmv ~ Month + Year + gmv + units + s1_fact.order_payment_type + 
               NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraTripod + 
               product_analytic_verticalFlashShoeAdapter, 
             data = train_cam_mult[,])
summary(model_4) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692
sort(vif(model_4))

#removing product_analytic_verticalFlashShoeAdapter having low significance
model_5 <-lm(formula = weekly_gmv ~ Month + Year + gmv + units + s1_fact.order_payment_type + 
               NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraTripod , 
             data = train_cam_mult[,])
summary(model_5) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692
sort(vif(model_5))

#removing units having relatively low significance
model_6 <-lm(formula = weekly_gmv ~ Month + Year + gmv + s1_fact.order_payment_type + 
               NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraTripod , 
             data = train_cam_mult[,])
summary(model_6) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692 
sort(vif(model_6))

#removing s1_fact.order_payment_type having relatively low significance
model_7 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
               NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraBatteryCharger + product_analytic_verticalCameraTripod , 
             data = train_cam_mult[,])
summary(model_7) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692
sort(vif(model_7))

#removing product_analytic_verticalCameraBatteryCharger having relatively low significance
model_8 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
               NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraTripod , 
             data = train_cam_mult[,])
summary(model_8) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692
sort(vif(model_8))

#removing OnlineMarketing having high VIF
model_9 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
               NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
               product_analytic_verticalCameraTripod , 
             data = train_cam_mult[,])
summary(model_9) #Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9692 
sort(vif(model_9))

#removing Radio having high VIF
model_10 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
                NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + ContentMarketing + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_10) #Multiple R-squared:  0.9686,	Adjusted R-squared:  0.9686
sort(vif(model_10))

#removing TotalInvestment having high VIF
model_11 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_11) #Multiple R-squared:  0.9684,	Adjusted R-squared:  0.9684  
sort(vif(model_11))

#removing Affiliates having high VIF
model_12 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_12) #Multiple R-squared:  0.9639,	Adjusted R-squared:  0.9639 
sort(vif(model_12))

#removing Sponsorship having high VIF
model_13 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
                NPS_Score + special_sales + 
                TV + Digital + ContentMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_13) #Multiple R-squared:  0.9601,	Adjusted R-squared:  0.9601 
sort(vif(model_13))

#removing SEM having high VIF
model_14 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
                NPS_Score + special_sales + 
                TV + Digital + ContentMarketing + 
                Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_14) #Multiple R-squared:  0.9582,	Adjusted R-squared:  0.9582 
sort(vif(model_14))

#ContentMarketing - VIF - 9.68 
#NPS_Score - VIF - 9.35
#Year - VIF - 6.68
#removing Year as it shows less impact on the R-squared
model_15 <-lm(formula = weekly_gmv ~ Month + gmv + 
                NPS_Score + special_sales + 
                TV + Digital + ContentMarketing + 
                Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_15) #Multiple R-squared:  0.9549,	Adjusted R-squared:  0.9549 
sort(vif(model_15))

#removing highinvestment having low significance
model_16 <-lm(formula = weekly_gmv ~ Month + gmv + 
                NPS_Score + special_sales + 
                TV + Digital + ContentMarketing + 
                Other + weekly_sold_units + highrevenue + 
                weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_16) #Multiple R-squared:  0.9549,	Adjusted R-squared:  0.9549 
sort(vif(model_16))

#removing gmv having low significance
model_17 <-lm(formula = weekly_gmv ~ Month + 
                NPS_Score + special_sales + 
                TV + Digital + ContentMarketing + 
                Other + weekly_sold_units + highrevenue + 
                weekly_avg_discount + product_analytic_verticalCameraBattery + 
                product_analytic_verticalCameraTripod , 
              data = train_cam_mult[,])
summary(model_17) #Multiple R-squared:  0.9549,	Adjusted R-squared:  0.9549 
sort(vif(model_17))

#ContentMarketing - VIF - 8.74
#NPS_Score - VIF - 7.04
#removing NPS_Score as it shows less impact on the R-squared
Mult_Cam_model_18 <-lm(formula = weekly_gmv ~ Month + 
                         special_sales + 
                         TV + Digital + ContentMarketing + 
                         Other + weekly_sold_units + highrevenue + 
                         weekly_avg_discount + product_analytic_verticalCameraBattery + 
                         product_analytic_verticalCameraTripod , 
                       data = train_cam_mult[,])
summary(Mult_Cam_model_18) #Multiple R-squared:  0.9446,	Adjusted R-squared:  0.9446   
sort(vif(Mult_Cam_model_18))
#All these 11 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- Mult_Cam_model_18
train_data <- train_cam_mult
test_data <- test_cam_mult
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2      RMSE       MAE       MAPE
# 0.9450916 0.1181253 0.0924233 0.05183936

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.059 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###KOYCK Model - to see the carry over effect#################

str(CameraAccessory_Model_1) #200069 obs. of  52 variables

#creating lag of n = 1 for the dependant variable - weekly_gmv
CameraAccessory_Koyck <- CameraAccessory_Model_1
CameraAccessory_Koyck$weekly_gmv_lag1<-lag(CameraAccessory_Koyck$weekly_gmv,n=1)
str(CameraAccessory_Koyck) #200069 obs. of  53 variables

#look for NA and omit
sum(is.na(CameraAccessory_Koyck)) #1 - first row
CameraAccessory_Koyck <- na.omit(CameraAccessory_Koyck)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Koyck), 0.7*nrow(CameraAccessory_Koyck))

#test and train data
train_cam_koyck=CameraAccessory_Koyck[indices,]
test_cam_koyck = CameraAccessory_Koyck[-indices,]

#model 1
k_model_1 <- lm(weekly_gmv~.,data=train_cam_koyck)
summary(k_model_1)
#Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
#2 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(k_model_1, direction="both")

# Run the step object
step

k_model_2 <- lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                  s1_fact.order_payment_type + Frequency + NPS_Score + special_sales + 
                  TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_2) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(k_model_2))

#removing Frequency with low significance and high VIF
k_model_3 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                 s1_fact.order_payment_type + NPS_Score + special_sales + 
                 TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                 OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                 highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_3) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_3))

#removing product_analytic_verticalCameraBatteryCharger with low significance
k_model_4 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                 s1_fact.order_payment_type + NPS_Score + special_sales + 
                 TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                 OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                 highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalFlash + product_analytic_verticalLens + 
                 product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_4) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_4))

#removing product_analytic_verticalFlash with low significance and high VIF
k_model_5 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                 s1_fact.order_payment_type + NPS_Score + special_sales + 
                 TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                 OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                 highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalLens + 
                 product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_5) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_5))

#removing product_analytic_verticalLens with low significance and high VIF
k_model_6 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                 s1_fact.order_payment_type + NPS_Score + special_sales + 
                 TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                 OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                 highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraBattery + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_6) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_6))

#removing product_analytic_verticalCameraBattery with low significance and high VIF
k_model_7 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                 s1_fact.order_payment_type + NPS_Score + special_sales + 
                 TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                 OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                 highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                 product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                 product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_7) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_7))

#removing product_analytic_verticalCameraTripod with low significance and high VIF
k_model_8 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                 s1_fact.order_payment_type + NPS_Score + special_sales + 
                 TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                 OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                 highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                 product_analytic_verticalFilter + 
                 product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_8) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_8))

#removing product_analytic_verticalFilter with low significance and high VIF
k_model_9 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                 s1_fact.order_payment_type + NPS_Score + special_sales + 
                 TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                 OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                 highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                 product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_9) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_9))

#removing product_analytic_verticalCameraBag with low significance
k_model_10 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                  s1_fact.order_payment_type + NPS_Score + special_sales + 
                  TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_10) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_10))

#removing s1_fact.order_payment_type with low significance and high VIF
k_model_11 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                  NPS_Score + special_sales + 
                  TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  product_analytic_verticalStrap + weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_11) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_11))

#removing product_analytic_verticalStrap with low significance
k_model_12 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                  NPS_Score + special_sales + 
                  TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_12) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_12))

#removing deliverycdays with low significance and high VIF
k_model_13 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
                  NPS_Score + special_sales + 
                  TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_13) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_13))

#removing deliverybdays with low significance
k_model_14 <-lm(formula = weekly_gmv ~ Month + Year + 
                  NPS_Score + special_sales + 
                  TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_14) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_14))

#removing TotalInvestment with high VIF
k_model_15 <-lm(formula = weekly_gmv ~ Month + Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_15) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_15))

#removing Other with low significance and high VIF
k_model_16 <-lm(formula = weekly_gmv ~ Month + Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + Sponsorship + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_16) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_16))

#removing Sponsorship with low significance and high VIF
k_model_17 <-lm(formula = weekly_gmv ~ Month + Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_17) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_17))

#removing Month with low significance
k_model_18 <-lm(formula = weekly_gmv ~ Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + Radio + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_18) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_18))

#removing Radio with low significance
k_model_19 <-lm(formula = weekly_gmv ~ Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + ContentMarketing + 
                  OnlineMarketing + Affiliates + SEM + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_19) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_19))

#removing OnlineMarketing with high VIF
k_model_20 <-lm(formula = weekly_gmv ~ Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + ContentMarketing + 
                  Affiliates + SEM + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_20) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_20))

#removing ContentMarketing with low significance
k_model_21 <-lm(formula = weekly_gmv ~ Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + 
                  Affiliates + SEM + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_21) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_21))

#removing SEM with high VIF
k_model_22 <-lm(formula = weekly_gmv ~ Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + 
                  Affiliates + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_22) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_22))

#weekly_gmv_lag1: VIF - 31
#weekly_sold_units: VIF - 24 - Removing this as it is not impacting the R squared 
k_model_23 <-lm(formula = weekly_gmv ~ Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + 
                  Affiliates + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_23) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996   
sort(vif(k_model_23))

#removing Affiliates with high VIF
k_model_24 <-lm(formula = weekly_gmv ~ Year + 
                  NPS_Score + special_sales + 
                  TV + Digital + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_24) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996   
sort(vif(k_model_24))

#removing NPS_Score with low significance
k_model_25 <-lm(formula = weekly_gmv ~ Year + 
                  special_sales + 
                  TV + Digital + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_25) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996   
sort(vif(k_model_25))

#removing TV with low significance
k_model_26 <-lm(formula = weekly_gmv ~ Year + 
                  special_sales + 
                  Digital + 
                  highrevenue + highinvestment + weekly_avg_discount + 
                  weekly_gmv_lag1, data = train_cam_koyck)
summary(k_model_26) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996   
sort(vif(k_model_26))

#removing highinvestment with low significance
k_Cam_model_27 <-lm(formula = weekly_gmv ~ Year + 
                      special_sales + 
                      Digital + 
                      highrevenue + weekly_avg_discount + 
                      weekly_gmv_lag1, data = train_cam_koyck)
summary(k_Cam_model_27) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996   
sort(vif(k_Cam_model_27))
#All these 6 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- k_Cam_model_27
train_data <- train_cam_koyck
test_data <- test_cam_koyck
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2      RMSE         MAE         MAPE
# 0.9999126 0.0504508 0.003323534 0.0006030499

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.005 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###Distributed lag - to capture the carry-over effect#################

str(CameraAccessory_Model_1) #200069 obs. of  52 variables

#creating lag of n = 1, 2 and 3 for the dependant variable
CameraAccessory_Dist <- CameraAccessory_Model_1
for(i in 1:3){
  temp_var <- lag(CameraAccessory_Dist$weekly_gmv,n=i)
  CameraAccessory_Dist<-cbind(CameraAccessory_Dist,temp_var)
  colnames(CameraAccessory_Dist)[names(CameraAccessory_Dist) == "temp_var"] <- paste("weekly_gmv_lag",i,sep="")
}
str(CameraAccessory_Dist) #200069 obs. of  55 variables

#creating lag of n = 1, 2 for the independant investement variables
inv_vars <- c("TV","Digital","Sponsorship","ContentMarketing","OnlineMarketing","Affiliates","SEM","Radio","Other")
for(i in 1:2){
  for(j in inv_vars){
    temp_var <- lag(CameraAccessory_Dist[,j],n=i)
    CameraAccessory_Dist<-cbind(CameraAccessory_Dist,temp_var)
    colnames(CameraAccessory_Dist)[names(CameraAccessory_Dist) == "temp_var"] <- paste(j,"_lag",i,sep="")
  }
}
str(CameraAccessory_Dist) #200069 obs. of  73 variables

#look for NA and omit
sum(is.na(CameraAccessory_Dist)) #33 - by introducing lag variables
CameraAccessory_Dist <- na.omit(CameraAccessory_Dist)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_Dist), 0.7*nrow(CameraAccessory_Dist))

#test and train data
train_cam_dist=CameraAccessory_Dist[indices,]
test_cam_dist = CameraAccessory_Dist[-indices,]

#model 1
D_model_1 <- lm(weekly_gmv~.,data=train_cam_dist)
summary(D_model_1)
#Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
#3 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(D_model_1, direction="both")

# Run the step object
step

D_model_2 <- lm(formula = weekly_gmv ~ Month + Year + gmv + s1_fact.order_payment_type + 
                  product_mrp + Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  product_analytic_verticalStrap + weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_2) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_2))

#removing gmv as it has low significance and relatively high VIF
D_model_3 <- lm(formula = weekly_gmv ~ Month + Year + s1_fact.order_payment_type + 
                  product_mrp + Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  product_analytic_verticalStrap + weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_3) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_3))

#removing discountpercentage as it has low significance and high VIF
D_model_4 <- lm(formula = weekly_gmv ~ Month + Year + s1_fact.order_payment_type + 
                  product_mrp + Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  product_analytic_verticalStrap + weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_4) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_4))

#removing product_mrp as it has low significance
D_model_5 <- lm(formula = weekly_gmv ~ Month + Year + s1_fact.order_payment_type + 
                  Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  product_analytic_verticalStrap + weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_5) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_5))

#removing product_analytic_verticalStrap due to low significance
D_model_6 <- lm(formula = weekly_gmv ~ Month + Year + s1_fact.order_payment_type + 
                  Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalFilter + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_6) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_6))

#removing product_analytic_verticalFilter due to low significance
D_model_7 <- lm(formula = weekly_gmv ~ Month + Year + s1_fact.order_payment_type + 
                  Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_7) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_7))

#removing s1_fact.order_payment_type due to low significance
D_model_8 <- lm(formula = weekly_gmv ~ Month + Year + 
                  Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + 
                  product_analytic_verticalFlash + product_analytic_verticalLens + 
                  weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_8) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_8))

#removing product_analytic_verticalFlash due to low significance and high VIF
D_model_9 <- lm(formula = weekly_gmv ~ Month + Year + 
                  Frequency + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                  product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                  product_analytic_verticalCameraTripod + product_analytic_verticalLens + 
                  weekly_gmv_lag1 + TV_lag1 + 
                  Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_cam_dist)
summary(D_model_9) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_9))

#removing Frequency due to low significance and high VIF
D_model_10 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                   product_analytic_verticalCameraBattery + product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalLens + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_10) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_10))

#removing product_analytic_verticalCameraBattery due to low significance and high VIF
D_model_11 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                   product_analytic_verticalCameraBatteryCharger + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalLens + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_11) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_11))

#removing product_analytic_verticalCameraTripod due to high VIF and low significance
D_model_12 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                   product_analytic_verticalCameraBatteryCharger + product_analytic_verticalLens + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_12) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_12))

#removing product_analytic_verticalLens due to high VIF and low significance
D_model_13 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                   product_analytic_verticalCameraBatteryCharger + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_13) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_13))

#removing product_analytic_verticalCameraBatteryCharger due to low significance
D_model_14 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraBag + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_14) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_14))

#removing product_analytic_verticalCameraBag due to low significance
D_model_15 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_15) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_15))

#removing SEM due to high VIF
D_model_16 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_16) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_16))

#removing TotalInvestment due to high VIF
D_model_17 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_17) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_17))

#removing Month due to low significance
D_model_18 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_18) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_18))

#removing OnlineMarketing_lag1 due to high VIF
D_model_19 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_19) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_19))

#removing TV_lag1 due to high VIF and low significance
D_model_20 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_20) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_20))

#removing SEM_lag1 due to low significance
D_model_21 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   Affiliates_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_21) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_21))

#removing Radio due to high VIF
D_model_22 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates  + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   Affiliates_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_22) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_22))

#removing ContentMarketing due to high VIF
D_model_23 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Affiliates  + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   Affiliates_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_23) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_23))

#removing Affiliates due to high VIF
D_model_24 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   Affiliates_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_24) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_24))

#removing Affiliates_lag1 due to high VIF
D_model_25 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 +  Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_25) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_25))

#removing Digital_lag1 due to high VIF
D_model_26 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 +  Radio_lag1 + 
                   Other_lag1, data = train_cam_dist)
summary(D_model_26) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_26))

#removing Other_lag1 due to high VIF
D_model_27 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 +  Radio_lag1, data = train_cam_dist)
summary(D_model_27) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_27))

#removing Sponsorship_lag1 due to high VIF
D_model_28 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 
                 + ContentMarketing_lag1 +  Radio_lag1, data = train_cam_dist)
summary(D_model_28) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_28))

#removing ContentMarketing_lag1 due to high VIF
D_model_29 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + OnlineMarketing + 
                   Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Radio_lag1, data = train_cam_dist)
summary(D_model_29) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_29))

#removing Sponsorship due to low signficance
D_model_30 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + OnlineMarketing + 
                   Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Radio_lag1, data = train_cam_dist)
summary(D_model_30) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_30))

#removing Other due to high VIF
D_model_31 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + OnlineMarketing + 
                   weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   Radio_lag1, data = train_cam_dist)
summary(D_model_31) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_31))

#removing Radio_lag1 due to low signficance
D_model_32 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + OnlineMarketing + 
                   weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1
                 , data = train_cam_dist)
summary(D_model_32) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_32))

#weekly_gmv_lag1 VIF = 29.2
#weekly_sold_units VIF = 23.9
#removing weekly_sold_units though the VIF is lower than weekly_gmv_lag1 as it is not impacting the R2.
D_model_33 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + OnlineMarketing + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1
                 , data = train_cam_dist)
summary(D_model_33) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_33))

#removing OnlineMarketing due to high VIF
D_model_34 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + 
                   TV + Digital + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1
                 , data = train_cam_dist)
summary(D_model_34) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_34))

#removing TV due to high VIF and low significance
D_model_35 <- lm(formula = weekly_gmv ~ Year + 
                   NPS_Score + special_sales + Digital + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1
                 , data = train_cam_dist)
summary(D_model_35) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_model_35))

#removing NPS_Score due to low significance
D_Cam_model_36 <- lm(formula = weekly_gmv ~ Year + 
                       special_sales + Digital + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1
                     , data = train_cam_dist)
summary(D_Cam_model_36) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_Cam_model_36))

#Although the highinvestment, highrevenue variables have less significance, we are retaining them 
#since the p value is less than 0.05 and the varibale is relevant to case study

#All these 7 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- D_Cam_model_36
train_data <- train_cam_dist
test_data <- test_cam_dist
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2       RMSE         MAE        MAPE
#  0.9998439 0.06689547 0.003363293 0.0006109946

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.007 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###multiplicative Distributed lag - to capture both interactive & carry-over effect#################

str(CameraAccessory_Model_1) #200069 obs. of  52 variables

#creating lag of n = 1, 2 and 3 for the dependant variable
CameraAccessory_MultDist <- CameraAccessory_Model_1
for(i in 1:3){
  temp_var <- lag(CameraAccessory_MultDist$weekly_gmv,n=i)
  CameraAccessory_MultDist<-cbind(CameraAccessory_MultDist,temp_var)
  colnames(CameraAccessory_MultDist)[names(CameraAccessory_MultDist) == "temp_var"] <- paste("weekly_gmv_lag",i,sep="")
}
str(CameraAccessory_MultDist) #200069 obs. of  55 variables

#creating lag of n = 1, 2 for the independant investement variables
inv_vars <- c("TV","Digital","Sponsorship","ContentMarketing","OnlineMarketing","Affiliates","SEM","Radio","Other")
for(i in 1:2){
  for(j in inv_vars){
    temp_var <- lag(CameraAccessory_MultDist[,j],n=i)
    CameraAccessory_MultDist<-cbind(CameraAccessory_MultDist,temp_var)
    colnames(CameraAccessory_MultDist)[names(CameraAccessory_MultDist) == "temp_var"] <- paste(j,"_lag",i,sep="")
  }
}
str(CameraAccessory_MultDist) #200069 obs. of  73 variables

#look for NA and omit
sum(is.na(CameraAccessory_MultDist)) #33 - by introducing lag variables
CameraAccessory_MultDist <- na.omit(CameraAccessory_MultDist)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(CameraAccessory_MultDist), 0.7*nrow(CameraAccessory_MultDist))

#test and train data
train_cam_multdist=CameraAccessory_MultDist[indices,]
test_cam_multdist = CameraAccessory_MultDist[-indices,]

#to avoid NAs while taking log values
train_cam_multdist[train_cam_multdist == 0] <- 0.00001
test_cam_multdist[test_cam_multdist == 0] <- 0.00001

#log of values
train_cam_multdist <- log(train_cam_multdist)
test_cam_multdist <- log(test_cam_multdist)

sum(is.na(train_cam_multdist))# no missing values
sum(is.na(test_cam_multdist))# no missing values

#model 1
MD_model_1 <- lm(weekly_gmv~.,data=train_cam_multdist)
summary(MD_model_1)
#Multiple R-squared:  Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
#3 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(MD_model_1, direction="both")

# Run the step object
step

MD_model_2 <- lm(formula = weekly_gmv ~ Month + Year + s1_fact.order_payment_type + 
                   product_mrp + product_procurement_sla + NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Digital_lag2 + Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 + SEM_lag2 + Other_lag2, 
                 data = train_cam_multdist)
summary(MD_model_2) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_2))

#removing s1_fact.order_payment_type as it has less significance
MD_model_3 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + product_procurement_sla + NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Digital_lag2 + Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 + SEM_lag2 + Other_lag2, 
                 data = train_cam_multdist)
summary(MD_model_3) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_3))

#removing Other_lag2 as it has less significance and relatively high VIF
MD_model_4 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + product_procurement_sla + NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Digital_lag2 + Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 + SEM_lag2 , 
                 data = train_cam_multdist)
summary(MD_model_4) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_4))

#removing product_mrp as it has less significance and relatively high VIF
MD_model_5 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_procurement_sla + NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Digital_lag2 + Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 + SEM_lag2 , 
                 data = train_cam_multdist)
summary(MD_model_5) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_5))

#removing product_procurement_sla as it has less significance
MD_model_6 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalCameraAccessory + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Digital_lag2 + Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 + SEM_lag2 , 
                 data = train_cam_multdist)
summary(MD_model_6) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_6))

#removing product_analytic_verticalCameraAccessory as it has less significance
MD_model_7 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Digital_lag2 + Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 + SEM_lag2 , 
                 data = train_cam_multdist)
summary(MD_model_7) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_7))

#removing SEM_lag2 as it has less significance
MD_model_8 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Digital_lag2 + Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 , 
                 data = train_cam_multdist)
summary(MD_model_8) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_8))

#removing Digital_lag2 as it has less significance
MD_model_9 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + 
                   TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                   OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + 
                   product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                   weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                   Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                   Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                   Sponsorship_lag2 + ContentMarketing_lag2 + 
                   OnlineMarketing_lag2 + Affiliates_lag2 , 
                 data = train_cam_multdist)
summary(MD_model_9) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_9))

#removing Sponsorship_lag2 as it has less significance
MD_model_10 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    product_analytic_verticalCameraTripod + product_analytic_verticalStrap + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    OnlineMarketing_lag2 + Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_10) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_10))

#removing product_analytic_verticalCameraTripod as it has less significance
MD_model_11 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    product_analytic_verticalStrap + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    OnlineMarketing_lag2 + Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_11) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_11))

#removing product_analytic_verticalStrap as it has less significance
MD_model_12 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    OnlineMarketing_lag2 + Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_12) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_model_12))

#removing Radio as it has less significance
MD_model_13 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    OnlineMarketing_lag2 + Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_13) #Multiple R-squared:  0.999,	Adjusted R-squared:  0.999 
sort(vif(MD_model_13))

#removing Other as it has less significance
MD_model_14 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + Affiliates + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + OnlineMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    OnlineMarketing_lag2 + Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_14) #Multiple R-squared:  0.999,	Adjusted R-squared:  0.999 
sort(vif(MD_model_14))

#removing OnlineMarketing_lag1 as it has high VIF
MD_model_15 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + Affiliates + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    OnlineMarketing_lag2 + Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_15) #Multiple R-squared:  0.999,	Adjusted R-squared:  0.999 
sort(vif(MD_model_15))

#removing OnlineMarketing_lag2 as it has high VIF
MD_model_16 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + Affiliates + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_16) #Multiple R-squared:  0.999,	Adjusted R-squared:  0.999 
sort(vif(MD_model_16))

#removing Affiliates as it has low significance and high VIF
MD_model_17 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 + 
                    Affiliates_lag2 , 
                  data = train_cam_multdist)
summary(MD_model_17) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
sort(vif(MD_model_17))

#removing Affiliates_lag2 as it has low significance and high VIF
MD_model_18 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 + 
                    ContentMarketing_lag2 , data = train_cam_multdist)
summary(MD_model_18) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
sort(vif(MD_model_18))

#removing ContentMarketing_lag2 as it has low significance
MD_model_19 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_19) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
sort(vif(MD_model_19))

#removing TotalInvestment as it has low significance
MD_model_20 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + 
                    TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_20) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
sort(vif(MD_model_20))

#removing Year as it has low significance
MD_model_21 <- lm(formula = weekly_gmv ~ Month + 
                    NPS_Score + special_sales + 
                    TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + Digital_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_21) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
sort(vif(MD_model_21))

#removing Digital_lag1 as it has high VIF
MD_model_22 <- lm(formula = weekly_gmv ~ Month + 
                    NPS_Score + special_sales + 
                    TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + TV_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_22) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
sort(vif(MD_model_22))

#removing TV_lag1 as it has high VIF
MD_model_23 <- lm(formula = weekly_gmv ~ Month + 
                    NPS_Score + special_sales + 
                    TV + Digital + Sponsorship + ContentMarketing + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_23) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989 
sort(vif(MD_model_23))

#removing ContentMarketing as it has high VIF
MD_model_24 <- lm(formula = weekly_gmv ~ Month + 
                    NPS_Score + special_sales + 
                    TV + Digital + Sponsorship + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_24) #Multiple R-squared:  0.9987,	Adjusted R-squared:  0.9987 
sort(vif(MD_model_24))

#removing Sponsorship as it has high VIF
MD_model_25 <- lm(formula = weekly_gmv ~ Month + 
                    NPS_Score + special_sales + 
                    TV + Digital + 
                    OnlineMarketing + SEM + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_25) #Multiple R-squared:  0.9987,	Adjusted R-squared:  0.9987 
sort(vif(MD_model_25))

#removing SEM as it has high VIF
MD_model_26 <- lm(formula = weekly_gmv ~ Month + 
                    NPS_Score + special_sales + 
                    TV + Digital + 
                    OnlineMarketing + weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_26) #Multiple R-squared:  0.9987,	Adjusted R-squared:  0.9987 
sort(vif(MD_model_26))

#removing OnlineMarketing as it has high VIF
MD_model_27 <- lm(formula = weekly_gmv ~ Month + 
                    NPS_Score + special_sales + 
                    TV + Digital + 
                    weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_27) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984
sort(vif(MD_model_27))

#removing Month as it has less significance
MD_model_28 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Radio_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_28) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984
sort(vif(MD_model_28))

#removing Radio_lag1 as it has high VIF
MD_model_29 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Other_lag1 + TV_lag2 , data = train_cam_multdist)
summary(MD_model_29) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984
sort(vif(MD_model_29))

#removing TV_lag2 as it has high VIF
MD_model_30 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + weekly_gmv_lag3 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Other_lag1 , data = train_cam_multdist)
summary(MD_model_30) #Multiple R-squared:  0.9958,	Adjusted R-squared:  0.9958 
sort(vif(MD_model_30))

#removing weekly_gmv_lag3 as it has low significance
MD_model_31 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    Affiliates_lag1 + SEM_lag1 + Other_lag1 , data = train_cam_multdist)
summary(MD_model_31) #Multiple R-squared:  0.9958,	Adjusted R-squared:  0.9958 
sort(vif(MD_model_31))

#removing Affiliates_lag1 as it has high VIF
MD_model_32 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    weekly_sold_units + 
                    highrevenue + highinvestment + weekly_avg_discount + 
                    weekly_gmv_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    SEM_lag1 + Other_lag1 , data = train_cam_multdist)
summary(MD_model_32) #Multiple R-squared:  0.9957,	Adjusted R-squared:  0.9957
sort(vif(MD_model_32))

#removing highinvestment as it has low significance
MD_model_33 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    weekly_sold_units + 
                    highrevenue + weekly_avg_discount + 
                    weekly_gmv_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    SEM_lag1 + Other_lag1 , data = train_cam_multdist)
summary(MD_model_33) #Multiple R-squared:  0.9957,	Adjusted R-squared:  0.9957
sort(vif(MD_model_33))

#weekly_gmv_lag1 - VIF - 20.5
##weekly_sold_units - VIF - 15.08 - removing weekly_sold_units as it shows less impact on R squared
MD_model_34 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    highrevenue + weekly_avg_discount + 
                    weekly_gmv_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    SEM_lag1 + Other_lag1 , data = train_cam_multdist)
summary(MD_model_34) #Multiple R-squared:  0.9952,	Adjusted R-squared:  0.9952
sort(vif(MD_model_34))

#removing Other_lag1 as it has less significance
MD_model_35 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    highrevenue + weekly_avg_discount + 
                    weekly_gmv_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    SEM_lag1 , data = train_cam_multdist)
summary(MD_model_35) #Multiple R-squared:  0.9952,	Adjusted R-squared:  0.9952 
sort(vif(MD_model_35))

#removing SEM_lag1 as it has high VIF
MD_model_36 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                    TV + Digital + 
                    highrevenue + weekly_avg_discount + 
                    weekly_gmv_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 , data = train_cam_multdist)
summary(MD_model_36) #Multiple R-squared:  0.9952,	Adjusted R-squared:  0.9952 
sort(vif(MD_model_36))

#removing ContentMarketing_lag1 as it has high VIF
MD_Cam_model_37 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                        TV + Digital + 
                        highrevenue + weekly_avg_discount + 
                        weekly_gmv_lag1 + 
                        Sponsorship_lag1 , data = train_cam_multdist)
summary(MD_Cam_model_37) #Multiple R-squared:  0.9952,	Adjusted R-squared:  0.9952 
sort(vif(MD_Cam_model_37))
#All these 8 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- MD_Cam_model_37
train_data <- train_cam_multdist
test_data <- test_cam_multdist
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2        RMSE       MAE        MAPE
#  0.999736 0.008228032 0.0019864 0.001143239

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.004 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

#######################################################################################################
########################################################################################################
###################### Home Audio ####################################
####-------simple linear regression -  -------####
str(Electronics_HomeAudio) #103198 obs. of  41 variables
HomeAudio_Model<-Electronics_HomeAudio[,-c(1,4,5,7,8,15:19)] #as these are not relevant to our study
str(HomeAudio_Model)  #103198 obs. of  31 variables

#Creating dummy variables
#product_analytic_vertical
dummy_vertical <-data.frame(model.matrix(~product_analytic_vertical, data=HomeAudio_Model))
dummy_vertical <- dummy_vertical[,-1]  

#creating levels
# order_payment_type
HomeAudio_Model$s1_fact.order_payment_type<-as.factor(HomeAudio_Model$s1_fact.order_payment_type)
levels(HomeAudio_Model$s1_fact.order_payment_type)<-c(1,0)
# Assigning 1 if the payment type is COD and 0 if Prepaid
HomeAudio_Model$s1_fact.order_payment_type<- as.numeric(levels(HomeAudio_Model$s1_fact.order_payment_type))[HomeAudio_Model$s1_fact.order_payment_type]

#combining dummy
HomeAudio_Model_1 <- cbind(HomeAudio_Model[,-2], dummy_vertical)

# View the new dataset 
View(HomeAudio_Model_1)

#scaling
HomeAudio_Model_1_scl<-scale(HomeAudio_Model_1)
HomeAudio_Model_1_scl<-as.data.frame(HomeAudio_Model_1)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Model_1_scl), 0.7*nrow(HomeAudio_Model_1_scl))

train_home_LR=HomeAudio_Model_1_scl[indices,]
test_home_LR = HomeAudio_Model_1_scl[-indices,]

# Develop the first model 
model_1 <-lm(weekly_gmv~.,data=train_home_LR[,])
summary(model_1)
# Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")

# Run the step object
step

#model 2
model_2 <-lm(formula = weekly_gmv ~ Month + Year + units + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + sla + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_2) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844
sort(vif(model_2))

#removing sla with high VIF and low significance
model_3 <-lm(formula = weekly_gmv ~ Month + Year + units + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_3) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844
sort(vif(model_3))

#removing units with low significance
model_4 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Radio + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_4) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844
sort(vif(model_4))

#removing Radio  with low significance
model_5 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_5) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844 
sort(vif(model_5))

#removing product_analytic_verticalDockingStation  with low significance
model_6 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_6) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844 
sort(vif(model_6))

#removing product_analytic_verticalHiFiSystem  with low significance and high VIF
model_7 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + 
               product_analytic_verticalFMRadio + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_7) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844 
sort(vif(model_7))

#removing product_analytic_verticalDock  with low significance and high VIF
model_8 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalFMRadio + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_8) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844 
sort(vif(model_8))

#removing product_analytic_verticalDJController with low significance
model_9 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + Frequency + 
               NPS_Score + special_sales + TotalInvestment + TV + Digital + 
               Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
               SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
               weekly_avg_discount + 
               product_analytic_verticalFMRadio + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalSoundMixer, 
             data = train_home_LR[, ])
summary(model_9) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844 
sort(vif(model_9))

#removing product_analytic_verticalSoundMixer  with low significance
model_10 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + Frequency + 
                NPS_Score + special_sales + TotalInvestment + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker , 
              data = train_home_LR[, ])
summary(model_10) #Multiple R-squared:  0.9844,	Adjusted R-squared:  0.9844 
sort(vif(model_10))

#removing TotalInvestment with high VIF
model_11 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + Frequency + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker , 
              data = train_home_LR[, ])
summary(model_11) #Multiple R-squared:  0.9843,	Adjusted R-squared:  0.9843
sort(vif(model_11))

#removing Month with low significance
model_12 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + Frequency + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker , 
              data = train_home_LR[, ])
summary(model_12) #Multiple R-squared:  0.9843,	Adjusted R-squared:  0.9843
sort(vif(model_12))

#removing product_analytic_verticalHomeAudioSpeaker with high VIF
model_13 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + Frequency + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount + 
                product_analytic_verticalFMRadio , data = train_home_LR[, ])
summary(model_13) #Multiple R-squared:  0.9843,	Adjusted R-squared:  0.9843 
sort(vif(model_13))

#removing Frequency with low significance
model_14 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount + 
                product_analytic_verticalFMRadio , data = train_home_LR[, ])
summary(model_14) #Multiple R-squared:  0.9843,	Adjusted R-squared:  0.9843
sort(vif(model_14))

#removing product_analytic_verticalFMRadio with low significance
model_15 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + Affiliates + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount , data = train_home_LR[, ])
summary(model_15) #Multiple R-squared:  0.9843,	Adjusted R-squared:  0.9843 
sort(vif(model_15))

#removing Affiliates with high VIF
model_16 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + Digital + 
                Sponsorship + ContentMarketing + OnlineMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount , data =  train_home_LR[,])
summary(model_16) #Multiple R-squared:  0.9842,	Adjusted R-squared:  0.9842 
sort(vif(model_16))

#removing Digital with high VIF
model_17 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + 
                Sponsorship + ContentMarketing + OnlineMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount , 
              data =  train_home_LR[,])
summary(model_17) #Multiple R-squared:  0.982,	Adjusted R-squared:  0.982 
sort(vif(model_17))

#removing ContentMarketing with high VIF
model_18 <-lm(formula = weekly_gmv ~ Year + deliverybdays + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + 
                Sponsorship + OnlineMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount , data =  train_home_LR[,])
summary(model_18) #Multiple R-squared:  0.9817,	Adjusted R-squared:  0.9817 
sort(vif(model_18))

#removing deliverybdays with high VIF
model_19 <-lm(formula = weekly_gmv ~ Year + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + 
                Sponsorship + OnlineMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount , data =  train_home_LR[,])
summary(model_19) #Multiple R-squared:  0.9817,	Adjusted R-squared:  0.9817 
sort(vif(model_19))

#removing Sponsorship with high VIF
model_20 <-lm(formula = weekly_gmv ~ Year + 
                deliverycdays + s1_fact.order_payment_type + 
                NPS_Score + special_sales + TV + OnlineMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount , data =  train_home_LR[,])
summary(model_20) #Multiple R-squared:  0.9805,	Adjusted R-squared:  0.9805
sort(vif(model_20))

#removing NPS_Score with high VIF
model_21 <-lm(formula = weekly_gmv ~ Year + 
                deliverycdays + s1_fact.order_payment_type + 
                special_sales + TV + OnlineMarketing + 
                SEM + Other + weekly_sold_units + highrevenue + highinvestment + 
                weekly_avg_discount , data =  train_home_LR[,])
summary(model_21) #Multiple R-squared:  0.9754,	Adjusted R-squared:  0.9754
sort(vif(model_21))

#removing SEM with low significance
LR_home_model_22 <-lm(formula = weekly_gmv ~ Year + 
                        deliverycdays + s1_fact.order_payment_type + 
                        special_sales + TV + OnlineMarketing + 
                        Other + weekly_sold_units + highrevenue + highinvestment + 
                        weekly_avg_discount , data =  train_home_LR[,])
summary(LR_home_model_22) #Multiple R-squared:  0.9754,	Adjusted R-squared:  0.9754
sort(vif(LR_home_model_22))
#All these 11 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- LR_home_model_22
train_data <- train_home_LR
test_data <- test_home_LR
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2     RMSE       MAE      MAPE
#  0.9751597 0.9799026 0.7323698 0.1136659

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
# 0.1112321 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

#########Multiplicative modelling - to see interactions between the KPIs#############

##Multiplicative modelling - HomeAudio#######

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Model_1), 0.7*nrow(HomeAudio_Model_1))
train_home_mult=HomeAudio_Model_1[indices,]
test_home_mult = HomeAudio_Model_1[-indices,]

#to avoid NAs while taking log values
train_home_mult[train_home_mult == 0] <- 0.00001
test_home_mult[test_home_mult == 0] <- 0.00001

#log of values
train_home_mult <- log(train_home_mult)
test_home_mult <- log(test_home_mult)

#check for no NA after the model
sum(is.na(train_home_mult)) 

# Develop the first model 
model_1 <-lm(weekly_gmv~.,data=train_home_mult[,])
summary(model_1)
#Multiple R-squared:  0.9772,	Adjusted R-squared:  0.9772

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")

# Run the step object
step

model_2 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               product_mrp + Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalKaraokePlayer + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_2) #Multiple R-squared:  0.9772,	Adjusted R-squared:  0.9772
sort(vif(model_2))

#removing product_mrp with low significance and high VIF
model_3 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalKaraokePlayer + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_3) #Multiple R-squared:  0.9772,	Adjusted R-squared:  0.9772
sort(vif(model_3))

#removing deliverybdays with high vif and low significance
model_4 <-lm(formula = weekly_gmv ~ Month + Year + gmv + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalKaraokePlayer + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_4) #Multiple R-squared:  0.9772,	Adjusted R-squared:  0.9772 
sort(vif(model_4))

#removing gmv with low significance
model_5 <-lm(formula = weekly_gmv ~ Month + Year + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalKaraokePlayer + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_5) #Multiple R-squared:  0.9772,	Adjusted R-squared:  0.9772
sort(vif(model_5))

#removing OnlineMarketing with high VIF
model_6 <-lm(formula = weekly_gmv ~ Month + Year + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalKaraokePlayer + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_6) #Multiple R-squared:  0.9767,	Adjusted R-squared:  0.9767 
sort(vif(model_6))

#removing ContentMarketing with low significance
model_7 <-lm(formula = weekly_gmv ~ Month + Year + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalKaraokePlayer + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_7) #Multiple R-squared:  0.9767,	Adjusted R-squared:  0.9767 
sort(vif(model_7))

#removing Radio with high VIF
model_8 <-lm(formula = weekly_gmv ~ Month + Year + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + 
               Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + product_analytic_verticalKaraokePlayer + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_8) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976 
sort(vif(model_8))

#removing product_analytic_verticalKaraokePlayer with low significance
model_9 <-lm(formula = weekly_gmv ~ Month + Year + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + 
               Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
               highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
               product_analytic_verticalDock + product_analytic_verticalDockingStation + 
               product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
               product_analytic_verticalHomeAudioSpeaker + 
               product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
             data = train_home_mult[,])
summary(model_9) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976 
sort(vif(model_9))

#removing product_analytic_verticalDockingStation with high vif and low significance
model_10 <-lm(formula = weekly_gmv ~ Month + Year + 
                Frequency + NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
                product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + product_analytic_verticalHiFiSystem + 
                product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
              data = train_home_mult[,])
summary(model_10) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976  
sort(vif(model_10))

#removing product_analytic_verticalHiFiSystem with low significance
model_11 <-lm(formula = weekly_gmv ~ Month + Year + 
                Frequency + NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + product_analytic_verticalDJController + 
                product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
              data = train_home_mult[,])
summary(model_11) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976 
sort(vif(model_11))

#removing product_analytic_verticalDJController with high vif and low significance
model_12 <-lm(formula = weekly_gmv ~ Month + Year + 
                Frequency + NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + 
                product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalSlingBox + product_analytic_verticalSoundMixer, 
              data = train_home_mult[,])
summary(model_12) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976 
sort(vif(model_12))

#remvoing product_analytic_verticalSlingBox with low significance
model_13 <-lm(formula = weekly_gmv ~ Month + Year + 
                Frequency + NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + 
                product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker + 
                product_analytic_verticalSoundMixer, 
              data = train_home_mult[,])
summary(model_13) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976
sort(vif(model_13))

#remvoing product_analytic_verticalSoundMixer with low significance and high VIF
model_14 <-lm(formula = weekly_gmv ~ Month + Year + 
                Frequency + NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + 
                product_analytic_verticalDock + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker, 
              data = train_home_mult[,])
summary(model_14) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976
sort(vif(model_14))

#remvoing product_analytic_verticalDock with low significance
model_15 <-lm(formula = weekly_gmv ~ Month + Year + 
                Frequency + NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker, 
              data = train_home_mult[,])
summary(model_15) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976
sort(vif(model_15))

#remvoing Frequency with low significance and high VIF
model_16 <-lm(formula = weekly_gmv ~ Month + Year + 
                NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + 
                product_analytic_verticalFMRadio + 
                product_analytic_verticalHomeAudioSpeaker, 
              data = train_home_mult[,])
summary(model_16) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976
sort(vif(model_16))

#remvoing product_analytic_verticalFMRadio with low significance
model_17 <-lm(formula = weekly_gmv ~ Month + Year + 
                NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount + 
                product_analytic_verticalHomeAudioSpeaker, 
              data = train_home_mult[,])
summary(model_17) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976
sort(vif(model_17))

#remvoing product_analytic_verticalHomeAudioSpeaker with low significance
model_18 <-lm(formula = weekly_gmv ~ Month + Year + 
                NPS_Score + special_sales + TotalInvestment + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount , 
              data = train_home_mult[,])
summary(model_18) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976
sort(vif(model_18))

#remvoing TotalInvestment with high VIF
model_19 <-lm(formula = weekly_gmv ~ Month + Year + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount , 
              data = train_home_mult[,])
summary(model_19) #Multiple R-squared:  0.976,	Adjusted R-squared:  0.976
sort(vif(model_19))

#remvoing Sponsorship with high VIF
model_20 <-lm(formula = weekly_gmv ~ Month + Year + 
                NPS_Score + special_sales + 
                TV + Digital + 
                Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount , 
              data = train_home_mult[,])
summary(model_20) #Multiple R-squared:  0.9724,	Adjusted R-squared:  0.9724
sort(vif(model_20))

#remvoing SEM with high VIF
model_21 <-lm(formula = weekly_gmv ~ Month + Year + 
                NPS_Score + special_sales + 
                TV + Digital + 
                Affiliates + Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount , 
              data = train_home_mult[,])
summary(model_21) #Multiple R-squared:  0.9697,	Adjusted R-squared:  0.9697
sort(vif(model_21))

#remvoing Affiliates with high VIF
model_22 <-lm(formula = weekly_gmv ~ Month + Year + 
                NPS_Score + special_sales + 
                TV + Digital + 
                Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount , 
              data = train_home_mult[,])
summary(model_22) #Multiple R-squared:  0.9566,	Adjusted R-squared:  0.9566 
sort(vif(model_22))

#remvoing Year with high VIF
Mult_home_model_23 <-lm(formula = weekly_gmv ~ Month + 
                NPS_Score + special_sales + 
                TV + Digital + 
                Other + weekly_sold_units + highrevenue + 
                highinvestment + weekly_avg_discount , 
              data = train_home_mult[,])
summary(Mult_home_model_23) #Multiple R-squared:  0.9566,	Adjusted R-squared:  0.9566 
sort(vif(Mult_home_model_23))
#All these 10 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- Mult_home_model_23
train_data <- train_home_mult
test_data <- test_home_mult
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2      RMSE       MAE       MAPE
# 0.9564928 0.11635 0.09302191 0.05284712

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.05808121 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

####-------KOYCK - Home Audio -------####
str(HomeAudio_Model_1) #103198 obs. of  40 variables

#creating lag of n = 1 for the dependant variable
HomeAudio_Koyck <- HomeAudio_Model_1
HomeAudio_Koyck$weekly_gmv_lag1<-lag(HomeAudio_Koyck$weekly_gmv,n=1)
str(HomeAudio_Koyck) #103198 obs. of  41 variables

#look for NA and omit
sum(is.na(HomeAudio_Koyck)) #1 - first row
HomeAudio_Koyck <- na.omit(HomeAudio_Koyck)

#scaling
HomeAudio_Koyck<-scale(HomeAudio_Koyck)
HomeAudio_Koyck<-as.data.frame(HomeAudio_Koyck)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Koyck), 0.7*nrow(HomeAudio_Koyck))

#test and train data
train_home_koyck = HomeAudio_Koyck[indices,]
test_home_koyck = HomeAudio_Koyck[-indices,]

# Develop the first model 
k_home_model_1 <-lm(weekly_gmv~.,data=train_home_koyck)
summary(k_home_model_1)
# Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 

# Apply the stepwise approach
step <- stepAIC(k_home_model_1, direction="both")

# Run the step object
step

k_home_model_2 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                      s1_fact.order_payment_type + NPS_Score + special_sales + 
                      TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      product_analytic_verticalHomeAudioSpeaker + weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_2) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_2))

#removing deliverycdays with low significance and relatively high VIF
k_home_model_3 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
                      s1_fact.order_payment_type + NPS_Score + special_sales + 
                      TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      product_analytic_verticalHomeAudioSpeaker + weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_3) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_3))

#removing s1_fact.order_payment_type with low significance and relatively high VIF
k_home_model_4 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
                      NPS_Score + special_sales + 
                      TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      product_analytic_verticalHomeAudioSpeaker + weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_4) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_4))

#removing product_analytic_verticalHomeAudioSpeaker with low significance
k_home_model_5 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
                      NPS_Score + special_sales + 
                      TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_5) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_5))

#removing Year with low significance and relatively high VIF
k_home_model_6 <-lm(formula = weekly_gmv ~ Month + deliverybdays + 
                      NPS_Score + special_sales + 
                      TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_6) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_6))

#removing Month with low significance
k_home_model_7 <-lm(formula = weekly_gmv ~ deliverybdays + 
                      NPS_Score + special_sales + 
                      TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_7) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_7))

#removing deliverybdays with low significance
k_home_model_8 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                      TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_8) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_8))

#removing TotalInvestment with high VIF
k_home_model_9 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                      TV + Digital + Sponsorship + ContentMarketing + 
                      OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                      highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                      weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_9) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_9))

#removing ContentMarketing with low significance and relatively high VIF
k_home_model_10 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + Sponsorship + 
                       OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_10) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_10))

#removing Sponsorship with low significance and relatively high VIF
k_home_model_11 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + 
                       OnlineMarketing + Affiliates + SEM + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_11) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_11))

#removing Other with low significance
k_home_model_12 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + 
                       OnlineMarketing + Affiliates + SEM + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_12) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_12))

#removing OnlineMarketing with high VIF 
k_home_model_13 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + 
                       Affiliates + SEM + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_13) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_13))

#removing SEM with high VIF
k_home_model_14 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + 
                       Affiliates + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_14) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_14))

#removing weekly_sold_units due to relarvely high VIF and has no impact on R squared
k_home_model_15 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + Affiliates + 
                       highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_15) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_15))

#removing highrevenue due to low significance
k_home_model_16 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + Affiliates + 
                       highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_16) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_16))

#removing Affiliates due to high VIF
k_home_model_17 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + 
                       highinvestment + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_17) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_17))

#removing highinvestment due to relatively high VIF and low significance
k_home_model_18 <-lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TV + Digital + 
                       weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_18) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_18))

#removing NPS_Score due to relatively high VIF and low significance
k_home_model_19 <-lm(formula = weekly_gmv ~ special_sales + 
                       TV + Digital + 
                       weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_19) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_19))

#removing TV low significance
k_home_model_20 <-lm(formula = weekly_gmv ~ special_sales + 
                       Digital + weekly_avg_discount + product_analytic_verticalDock + 
                       weekly_gmv_lag1, data = train_home_koyck)
summary(k_home_model_20) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(k_home_model_20))
vif(k_home_model_20)
#All these 5 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- k_home_model_20
train_data <- train_home_koyck
test_data <- test_home_koyck
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2      RMSE         MAE         MAPE
# 0.9997654 0.01526234 0.0009934873 0.0077361

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#-3.817791- shows low error rate #negative values in test data after scaling lead to negative mean

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###Distributed lag - to capture the carry-over effect#################

str(HomeAudio_Model_1) #103198 obs. of  40 variables

#creating lag of n = 1, 2 and 3 for the dependant variable
HomeAudio_Dist <- HomeAudio_Model_1
for(i in 1:3){
  temp_var <- lag(HomeAudio_Dist$weekly_gmv,n=i)
  HomeAudio_Dist<-cbind(HomeAudio_Dist,temp_var)
  colnames(HomeAudio_Dist)[names(HomeAudio_Dist) == "temp_var"] <- paste("weekly_gmv_lag",i,sep="")
}
str(HomeAudio_Dist) #103198 obs. of  43 variables

#creating lag of n = 1, 2 for the independant investement variables
inv_vars <- c("TV","Digital","Sponsorship","ContentMarketing","OnlineMarketing","Affiliates","SEM","Radio","Other")
for(i in 1:2){
  for(j in inv_vars){
    temp_var <- lag(HomeAudio_Dist[,j],n=i)
    HomeAudio_Dist<-cbind(HomeAudio_Dist,temp_var)
    colnames(HomeAudio_Dist)[names(HomeAudio_Dist) == "temp_var"] <- paste(j,"_lag",i,sep="")
  }
}
str(HomeAudio_Dist) #103198 obs. of  61 variables

#look for NA and omit
sum(is.na(HomeAudio_Dist)) #33 - by introducing lag variables
HomeAudio_Dist <- na.omit(HomeAudio_Dist)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_Dist), 0.7*nrow(HomeAudio_Dist))

#test and train data
train_home_dist=HomeAudio_Dist[indices,]
test_home_dist = HomeAudio_Dist[-indices,]

#model 1
D_home_model_1 <- lm(weekly_gmv~.,data=train_home_dist)
summary(D_home_model_1)
#Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
#3 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(D_home_model_1, direction="both")

# Run the step object
step

D_home_model_2 <- lm(formula = weekly_gmv ~ Month + Year + deliverybdays + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_2) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997 
sort(vif(D_home_model_2))

#removing Year as it has low significance
D_home_model_3 <- lm(formula = weekly_gmv ~ Month + deliverybdays + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_3) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_3))

#removing Month as it has low significance 
D_home_model_4 <- lm(formula = weekly_gmv ~ deliverybdays + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_4) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_4))

#removing deliverybdays as it has low significance 
D_home_model_5 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_5) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_5))

#removing SEM as it has high vif 
D_home_model_6 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_6) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_6))

#removing Sponsorship as it has low signifcance
D_home_model_7 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + ContentMarketing + 
                       OnlineMarketing + Affiliates + Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_7) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_7))

#removing OnlineMarketing as it has high vif 
D_home_model_8 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + ContentMarketing + 
                       Affiliates + Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_8) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_8))


#removing Affiliates as it has low signifcance and relatively high VIF
D_home_model_9 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + ContentMarketing + 
                       Radio + Other + weekly_sold_units + 
                       highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1, data = train_home_dist)
summary(D_home_model_9) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_9))

#removing SEM_lag1 as it has high vif
D_home_model_10 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + ContentMarketing + 
                        Radio + Other + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Affiliates_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_10) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_home_model_10))

#removing Affiliates_lag1 as it has low signifcance
D_home_model_11 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + ContentMarketing + 
                        Radio + Other + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_11) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997 
sort(vif(D_home_model_11))

#removing Radio as it has low signifcance and high vif
D_home_model_12 <-  lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                         TotalInvestment + TV + Digital + ContentMarketing + 
                         Other + weekly_sold_units + 
                         highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                         TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                         OnlineMarketing_lag1 + Radio_lag1 + 
                         Other_lag1, data = train_home_dist)
summary(D_home_model_12) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_12))

#removing TV as it has low significance
D_home_model_13 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                        TotalInvestment + Digital + ContentMarketing + 
                        Other + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_13) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_13))

#removing Digital_lag1 as it has low significance
D_home_model_14 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                        TotalInvestment + Digital + ContentMarketing + 
                        Other + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_14) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_14))

#removing NPS_Score as it has low significance
D_home_model_15 <- lm(formula = weekly_gmv ~ special_sales + 
                        TotalInvestment + Digital + ContentMarketing + 
                        Other + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_15) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_15))

#removing ContentMarketing as it has high VIF
D_home_model_16 <- lm(formula = weekly_gmv ~ special_sales + 
                        TotalInvestment + Digital + 
                        Other + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_16) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_16))

#removing ContentMarketing_lag1 as it has low significance 
D_home_model_17 <- lm(formula = weekly_gmv ~ special_sales + 
                        TotalInvestment + Digital + 
                        Other + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Sponsorship_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_17) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_17))

#removing Other as it has high vif
D_home_model_18 <- lm(formula = weekly_gmv ~ special_sales + 
                        TotalInvestment + Digital + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Sponsorship_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_18) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_18))

#removing TotalInvestment as it has high vif
D_home_model_19 <- lm(formula = weekly_gmv ~ special_sales + 
                        Digital + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + Sponsorship_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_19) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_19))

#removing Sponsorship_lag1 as it has low significance and relatively hihg VIF
D_home_model_20 <- lm(formula = weekly_gmv ~ special_sales + 
                        Digital + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_20) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_20))

#removing Digital as it has low significance
D_home_model_21 <- lm(formula = weekly_gmv ~ special_sales + weekly_sold_units + 
                        highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_21) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996
sort(vif(D_home_model_21))

#removing highinvestment as it has relatively high vif
D_home_model_22 <- lm(formula = weekly_gmv ~ special_sales + weekly_sold_units + 
                        highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1, data = train_home_dist)
summary(D_home_model_22) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_home_model_22))

#removing Other_lag1 as it has relatively high vif
D_home_model_23 <- lm(formula = weekly_gmv ~ special_sales + weekly_sold_units + 
                        highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                        TV_lag1 + OnlineMarketing_lag1 + Radio_lag1 , data = train_home_dist)
summary(D_home_model_23) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_home_model_23))

#removing weekly_avg_discount as it has low significance and relatively high vif
D_home_model_24 <- lm(formula = weekly_gmv ~ special_sales + weekly_sold_units + 
                        highrevenue + weekly_gmv_lag1 + 
                        TV_lag1 + OnlineMarketing_lag1 + Radio_lag1 , data = train_home_dist)
summary(D_home_model_24) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_home_model_24))

#removing OnlineMarketing_lag1 as it has low significance and relatively high vif
D_home_model_25 <- lm(formula = weekly_gmv ~ special_sales + weekly_sold_units + 
                        highrevenue + weekly_gmv_lag1 + 
                        TV_lag1 + Radio_lag1 , data = train_home_dist)
summary(D_home_model_25) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_home_model_25))

#removing Radio_lag1 as it has low significance 
D_home_model_26 <- lm(formula = weekly_gmv ~ special_sales + weekly_sold_units + 
                        highrevenue + weekly_gmv_lag1 + 
                        TV_lag1 , data = train_home_dist)
summary(D_home_model_26) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_home_model_26))

#removing weekly_sold_units as it has relatively high VIF and shows less impact on R squared
D_home_model_27 <- lm(formula = weekly_gmv ~ special_sales + 
                        highrevenue + weekly_gmv_lag1 + 
                        TV_lag1 , data = train_home_dist)
summary(D_home_model_27) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_home_model_27))

#removing TV_lag1 as it has low significance
D_home_model_28 <- lm(formula = weekly_gmv ~ special_sales + 
                        highrevenue + weekly_gmv_lag1 , data = train_home_dist)
summary(D_home_model_28) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(D_home_model_28))
#All these 3 variables are significant(p<0.05) with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- D_home_model_28
train_data <- train_home_dist
test_data <- test_home_dist
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#         R2       RMSE         MAE        MAPE
#  0.9994103 0.1508429 0.003468221 0.0005550256

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.01707712 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###multiplicative Distributed lag - to capture both interactive & carry-over effect#################

str(HomeAudio_Model_1) #103198 obs. of  40 variables

#creating lag of n = 1, 2 and 3 for the dependant variable
HomeAudio_MultDist <- HomeAudio_Model_1
for(i in 1:3){
  temp_var <- lag(HomeAudio_MultDist$weekly_gmv,n=i)
  HomeAudio_MultDist<-cbind(HomeAudio_MultDist,temp_var)
  colnames(HomeAudio_MultDist)[names(HomeAudio_MultDist) == "temp_var"] <- paste("weekly_gmv_lag",i,sep="")
}
str(HomeAudio_MultDist) #103198 obs. of  43 variables

#creating lag of n = 1, 2 for the independent investement variables
inv_vars <- c("TV","Digital","Sponsorship","ContentMarketing","OnlineMarketing","Affiliates","SEM","Radio","Other")
for(i in 1:2){
  for(j in inv_vars){
    temp_var <- lag(HomeAudio_MultDist[,j],n=i)
    HomeAudio_MultDist<-cbind(HomeAudio_MultDist,temp_var)
    colnames(HomeAudio_MultDist)[names(HomeAudio_MultDist) == "temp_var"] <- paste(j,"_lag",i,sep="")
  }
}
str(HomeAudio_MultDist) #103198 obs. of  61 variables

#look for NA and omit
sum(is.na(HomeAudio_MultDist)) #33 - by introducing lag variables
HomeAudio_MultDist <- na.omit(HomeAudio_MultDist)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_MultDist), 0.7*nrow(HomeAudio_MultDist))

#test and train data
train_home_multdist=HomeAudio_MultDist[indices,]
test_home_multdist = HomeAudio_MultDist[-indices,]

#to avoid NAs while taking log values
train_home_multdist[train_home_multdist == 0] <- 0.00001
test_home_multdist[test_home_multdist == 0] <- 0.00001

#log of values
train_home_multdist <- log(train_home_multdist)
test_home_multdist <- log(test_home_multdist)

sum(is.na(train_home_multdist))# no missing values
sum(is.na(test_home_multdist))# no missing values

#model 1
MD_home_model_1 <- lm(weekly_gmv~.,data=train_home_multdist)
summary(MD_home_model_1)
#Multiple R-squared:  Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
#3 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(MD_home_model_1, direction="both")

# Run the step object
step

MD_home_model_2 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                       weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
                       product_analytic_verticalHomeAudioSpeaker + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_2) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_home_model_2))

#removing weekly_avg_discount as it has low significance and relatively high VIF
MD_home_model_3 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                       weekly_sold_units + highrevenue + highinvestment +  
                       product_analytic_verticalHomeAudioSpeaker + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_3) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_home_model_3))

#removing deliverybdays as it has low significance 
MD_home_model_4 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                       weekly_sold_units + highrevenue + highinvestment +  
                       product_analytic_verticalHomeAudioSpeaker + weekly_gmv_lag1 + 
                       TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_4) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_home_model_4))

#removing product_analytic_verticalHomeAudioSpeaker as it has low significance 
MD_home_model_5 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                       weekly_sold_units + highrevenue + highinvestment +  
                       weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_5) #Multiple R-squared:  0.9991,	Adjusted R-squared:  0.9991 
sort(vif(MD_home_model_5))

#removing OnlineMarketing as it has relatively high VIF
MD_home_model_6 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       Affiliates + SEM + Radio + Other + discountpercentage + 
                       weekly_sold_units + highrevenue + highinvestment +  
                       weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_6) #Multiple R-squared:  0.999,	Adjusted R-squared:  0.999 
sort(vif(MD_home_model_6))

#removing discountpercentage as it has relatively low significance 
MD_home_model_7 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       Affiliates + SEM + Radio + Other + 
                       weekly_sold_units + highrevenue + highinvestment +  
                       weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_7) #Multiple R-squared:  0.999,	Adjusted R-squared:  0.999 
sort(vif(MD_home_model_7))

#removing Radio as it has relatively high VIF
MD_home_model_8 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       Affiliates + SEM + Other + 
                       weekly_sold_units + highrevenue + highinvestment +  
                       weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_8) #Multiple R-squared:  0.9989,	Adjusted R-squared:  0.9989
sort(vif(MD_home_model_8))

#removing Affiliates_lag1 as it has high vif
MD_home_model_9 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                       TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                       Affiliates + SEM + Other + 
                       weekly_sold_units + highrevenue + highinvestment +  
                       weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                       OnlineMarketing_lag1 + SEM_lag1 + Radio_lag1 + 
                       Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_9) #Multiple R-squared:  0.9986,	Adjusted R-squared:  0.9986  
sort(vif(MD_home_model_9))

#removing Other as it has relatively low significance
MD_home_model_10 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + SEM_lag1 + Radio_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_10) #Multiple R-squared:  0.9986,	Adjusted R-squared:  0.9986 
sort(vif(MD_home_model_10))

#removing SEM_lag1 as it has  high vif
MD_home_model_11 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_11) #Multiple R-squared:  0.9986,	Adjusted R-squared:  0.9986 
sort(vif(MD_home_model_11))

#removing Sponsorship as it has high vif
MD_home_model_12 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + ContentMarketing + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        OnlineMarketing_lag1 + Radio_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_12) #Multiple R-squared:  0.9985,	Adjusted R-squared:  0.9985 
sort(vif(MD_home_model_12))

#removing OnlineMarketing_lag1 as it has  high vif
MD_home_model_13 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + ContentMarketing + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        Radio_lag1 + Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_13) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984 
sort(vif(MD_home_model_13))

#removing ContentMarketing as it has  high vif and relatively low signifance
MD_home_model_14 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                        Radio_lag1 + Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_14) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984
sort(vif(MD_home_model_14))

#removing ContentMarketing_lag1 as it has  relatively low signifance
MD_home_model_15 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + Digital + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Radio_lag1 + Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_15) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984 
sort(vif(MD_home_model_15))

#removing Digital as it has  high vif
MD_home_model_16 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Radio_lag1 + Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_16) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984 
sort(vif(MD_home_model_16))

#removing Radio_lag1 as it has  high vif 
MD_home_model_17 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TotalInvestment + TV + 
                        Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_17) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984 
sort(vif(MD_home_model_17))

#removing TotalInvestment as it has low significance
MD_home_model_18 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TV + Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + TV_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_18) #Multiple R-squared:  0.9984,	Adjusted R-squared:  0.9984
sort(vif(MD_home_model_18))

#removing TV_lag1 as it has high vif 
MD_home_model_19 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TV + Affiliates + SEM + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_19) #Multiple R-squared:  0.9982,	Adjusted R-squared:  0.9982 
sort(vif(MD_home_model_19))

#removing SEM as it has relatively high vif and shows less impact on R squared
MD_home_model_20 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TV + Affiliates + 
                        weekly_sold_units + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_20) #Multiple R-squared:  0.9981,	Adjusted R-squared:  0.9981 
sort(vif(MD_home_model_20))

#removing weekly_sold_units as it has relatively high vif and shows less impact on R squared 
MD_home_model_21 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TV + Affiliates + 
                        highrevenue + highinvestment +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_21) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_21))

#removing Affiliates as it has low significance and relatively high vif
MD_home_model_22 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TV + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 + 
                        Other_lag1,data = train_home_multdist[,])
summary(MD_home_model_22) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_22))

#removing Other_lag1 as it has low significance
MD_home_model_23 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TV + highrevenue + highinvestment +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 ,data = train_home_multdist[,])
summary(MD_home_model_23) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_23))

#removing highinvestment as it has low significance
MD_home_model_24 <-lm(formula = weekly_gmv ~ Month + Year + NPS_Score + special_sales + 
                        TV + highrevenue +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 ,data = train_home_multdist[,])
summary(MD_home_model_24) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_24))

#removing NPS_Score as it has low significance
MD_home_model_25 <-lm(formula = weekly_gmv ~ Month + Year + special_sales + 
                        TV + highrevenue +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 ,data = train_home_multdist[,])
summary(MD_home_model_25) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_25))

#removing Month as it has low significance and relatively high VIF
MD_home_model_26 <-lm(formula = weekly_gmv ~ Year + special_sales + 
                        TV + highrevenue +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 ,data = train_home_multdist[,])
summary(MD_home_model_26) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_26))

#removing Year as it has low significance
MD_home_model_27 <-lm(formula = weekly_gmv ~ special_sales + 
                        TV + highrevenue +  
                        weekly_gmv_lag1 + Digital_lag1 + Sponsorship_lag1 ,data = train_home_multdist[,])
summary(MD_home_model_27) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_27))

#removing Sponsorship_lag1 as it has low significance
MD_home_model_28 <-lm(formula = weekly_gmv ~ special_sales + 
                        TV + highrevenue +  
                        weekly_gmv_lag1 + Digital_lag1 ,data = train_home_multdist[,])
summary(MD_home_model_28) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_28))

#removing Digital_lag1 as it has low significance
MD_home_model_29 <-lm(formula = weekly_gmv ~ special_sales + 
                        TV + highrevenue +  
                        weekly_gmv_lag1 ,data = train_home_multdist[,])
summary(MD_home_model_29) #Multiple R-squared:  0.998,	Adjusted R-squared:  0.998 
sort(vif(MD_home_model_29))
#All these 4 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- MD_home_model_29
train_data <- train_home_multdist
test_data <- test_home_multdist
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#       R2       RMSE         MAE         MAPE
# 0.999343 0.01430914 0.000941256 0.0005507867

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.007131867- shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

########################################################################################################
########################################################################################################
###################### Gaming Accessory ####################################

####-------simple linear regression -------####
str(Electronics_GamingAccessory) #175478 obs. of  41 variables
GamingAccessory_Model<-Electronics_GamingAccessory[,-c(1,4,5,7,8,15:19)] #as these are not relevant to our study
str(GamingAccessory_Model)  #175478 obs. of  31 variables

#Creating dummy variables
#product_analytic_vertical
dummy_vertical <-data.frame(model.matrix(~product_analytic_vertical, data=GamingAccessory_Model))
dummy_vertical <- dummy_vertical[,-1]  

#creating levels
# order_payment_type
GamingAccessory_Model$s1_fact.order_payment_type<-as.factor(GamingAccessory_Model$s1_fact.order_payment_type)
levels(GamingAccessory_Model$s1_fact.order_payment_type)<-c(1,0)
# Assigning 1 if the payment type is COD and 0 if Prepaid
GamingAccessory_Model$s1_fact.order_payment_type<- as.numeric(levels(GamingAccessory_Model$s1_fact.order_payment_type))[GamingAccessory_Model$s1_fact.order_payment_type]

#combining dummy
GamingAccessory_Model_1 <- cbind(GamingAccessory_Model[,-2], dummy_vertical)

# View the new dataset carprice_1
View(GamingAccessory_Model_1)

#scaling
GamingAccessory_Model_1_scl<-scale(GamingAccessory_Model_1)
GamingAccessory_Model_1_scl<-as.data.frame(GamingAccessory_Model_1)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Model_1_scl), 0.7*nrow(GamingAccessory_Model_1_scl))

train_gameacc_LR = GamingAccessory_Model_1_scl[indices,]
test_gameacc_LR = GamingAccessory_Model_1_scl[-indices,]

# Develop the first model 
LR_model_1 <-lm(weekly_gmv~.,data=train_gameacc_LR[,])
summary(LR_model_1)
#Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794

# Apply the stepwise approach
step <- stepAIC(LR_model_1, direction="both")

# Run the step object
step

#model 2
model_2 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_mrp + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingAccessoryKit + product_analytic_verticalGamingAdapter + 
               product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalJoystickGamingWheel + product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_2) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794 
sort(vif(model_2))

#removing product_analytic_verticalGamingAdapter as it is less significant and has relatively high VIF
model_3 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_mrp + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingAccessoryKit + 
               product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalJoystickGamingWheel + product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_3) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794  
sort(vif(model_3))

#removing product_analytic_verticalJoystickGamingWheel as it is less significant and high VIF
model_4 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_mrp + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingAccessoryKit + 
               product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_4) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794
sort(vif(model_4))

#removing product_analytic_verticalGamingAccessoryKit as it is less significant and has relatively high VIF
model_5 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_mrp + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingHeadset + product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_5) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794
sort(vif(model_5))

#removing product_analytic_verticalGamingHeadset as it is less significant and high VIF
model_6 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_mrp + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_6) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794
sort(vif(model_6))

#removing product_analytic_verticalGamingMousePad as it is less significant
model_7 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_mrp + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + 
               product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_7) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794 
sort(vif(model_7))

#removing product_mrp as it is less significant and high VIF
model_8 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + 
               product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_8) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794
sort(vif(model_8))

#removing discountpercentage as it is less significant
model_9 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
               s1_fact.order_payment_type + sla + product_procurement_sla + 
               Frequency + NPS_Score + special_sales + TotalInvestment + 
               TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
               Affiliates + SEM + Radio + Other + weekly_sold_units + 
               highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
               product_analytic_verticalGamingKeyboard + 
               product_analytic_verticalGamingMouse + 
               product_analytic_verticalMotionController, 
             data = train_gameacc_LR[,])
summary(model_9) #Multiple R-squared:  0.9794,	Adjusted R-squared:  0.9794 
sort(vif(model_9))

#removing TotalInvestment as it has high VIF
model_10 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + sla + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                Affiliates + SEM + Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse + 
                product_analytic_verticalMotionController, 
              data = train_gameacc_LR[,])
summary(model_10) #Multiple R-squared:  0.9793,	Adjusted R-squared:  0.9793  
sort(vif(model_10))

#removing SEM as it has high VIF
model_11 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + sla + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                Affiliates + Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse + 
                product_analytic_verticalMotionController, 
              data = train_gameacc_LR[,])
summary(model_11) #Multiple R-squared:  0.979,	Adjusted R-squared:  0.979  
sort(vif(model_11))

#removing sla as it has low significance
model_12 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                Affiliates + Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse + 
                product_analytic_verticalMotionController, 
              data = train_gameacc_LR[,])
summary(model_12) #Multiple R-squared:  0.979,	Adjusted R-squared:  0.979 
sort(vif(model_12))

#removing product_analytic_verticalMotionController as it has less significance
model_13 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                Affiliates + Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_13) #Multiple R-squared:  0.979,	Adjusted R-squared:  0.979 
sort(vif(model_13))

#removing Affiliates as it has high VIF
model_14 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                Radio + Other + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_14) #Multiple R-squared:  0.979,	Adjusted R-squared:  0.979
sort(vif(model_14))

#removing Other as it has high VIF
model_15 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_15) #Multiple R-squared:  0.9777,	Adjusted R-squared:  0.9777  
sort(vif(model_15))

#removing product_analytic_verticalGamePad as it is less significant
model_16 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_16) #Multiple R-squared:  0.9777,	Adjusted R-squared:  0.9777 
sort(vif(model_16))

#removing ContentMarketing as it has high VIF
model_17 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + product_procurement_sla + 
                Frequency + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_17) #Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9768
sort(vif(model_17))

#removing Frequency as it has low significance and relatively high VIF
model_18 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + product_procurement_sla + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_18) #Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9768 
sort(vif(model_18))

#removing product_procurement_sla as it has low significance and relatively high VIF
model_19 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingKeyboard + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_19) #Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9768 
sort(vif(model_19))

#removing product_analytic_verticalGamingKeyboard as it has low significance
model_20 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + deliverycdays + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_20) #Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9768 
sort(vif(model_20))

#removing deliverycdays as it has high VIF
model_21 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_21) #Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9768 
sort(vif(model_21))

#removing deliverybdays as it has low significance
model_22 <-lm(formula = weekly_gmv ~ Month + Year + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_22) #Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9768 
sort(vif(model_22))

#removing Month as it has high VIF
model_23 <-lm(formula = weekly_gmv ~ Year + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + 
                TV + Digital + Sponsorship + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_23) #Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9763
sort(vif(model_23))

#removing Sponsorship as it has high VIF
model_24 <-lm(formula = weekly_gmv ~ Year + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + 
                TV + Digital + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingMouse , 
              data = train_gameacc_LR[,])
summary(model_24) #Multiple R-squared:  0.9725,	Adjusted R-squared:  0.9725 
sort(vif(model_24))

#removing product_analytic_verticalGamingMouse as it has low significance
model_25 <-lm(formula = weekly_gmv ~ Year + 
                s1_fact.order_payment_type + 
                NPS_Score + special_sales + 
                TV + Digital + OnlineMarketing + 
                Radio + weekly_sold_units + 
                highrevenue + highinvestment + weekly_avg_discount , 
              data = train_gameacc_LR[,])
summary(model_25) #Multiple R-squared:  0.9725,	Adjusted R-squared:  0.9725 
sort(vif(model_25))

#removing NPS_Score as it has high VIF
LR_GamingAccessory_model_26 <-lm(formula = weekly_gmv ~ Year + 
                                   s1_fact.order_payment_type + 
                                   special_sales + 
                                   TV + Digital + OnlineMarketing + 
                                   Radio + weekly_sold_units + 
                                   highrevenue + highinvestment + weekly_avg_discount , 
                                 data = train_gameacc_LR[,])
summary(LR_GamingAccessory_model_26) #Multiple R-squared:  0.9653,	Adjusted R-squared:  0.9653  
sort(vif(LR_GamingAccessory_model_26))
#All these 11 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- LR_GamingAccessory_model_26
train_data <- train_gameacc_LR
test_data <- test_gameacc_LR
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#         R2     RMSE       MAE      MAPE
#  0.9660717 1.069885 0.8328117 0.1969334

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.1212648 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

##Multiplicative modelling - GameAcc#######

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Model_1), 0.7*nrow(GamingAccessory_Model_1))
train_Game_mult = GamingAccessory_Model_1[indices,]
test_Game_mult = GamingAccessory_Model_1[-indices,]

#to avoid NAs while taking log values
train_Game_mult[train_Game_mult == 0] <- 0.00001
test_Game_mult[test_Game_mult == 0] <- 0.00001

#log of values
train_Game_mult <- log(train_Game_mult)
test_Game_mult <- log(test_Game_mult)

#check for no NA after the model
sum(is.na(train_Game_mult)) # no missing values

# Develop the first model 
model_1 <-lm(weekly_gmv~.,data=train_Game_mult[,])
summary(model_1)
#Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 

# Apply the stepwise approach
step <- stepAIC(model_1, direction="both")

# Run the step object
step

model_2 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               deliverycdays + s1_fact.order_payment_type + sla + product_mrp + 
               product_procurement_sla + Frequency + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + product_analytic_verticalGamingHeadset + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, data = train_Game_mult[,])
summary(model_2) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_2))

#removing deliverycdays with high vif and low significance
model_3 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               s1_fact.order_payment_type + sla + product_mrp + 
               product_procurement_sla + Frequency + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + product_analytic_verticalGamingHeadset + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, data = train_Game_mult[,])
summary(model_3) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_3))

#removing s1_fact.order_payment_type with low significance
model_4 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               sla + product_mrp + 
               product_procurement_sla + Frequency + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + product_analytic_verticalGamingHeadset + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, data = train_Game_mult[,])
summary(model_4) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_4))

#removing product_analytic_verticalGamingHeadset with high vif and low significance
model_5 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               sla + product_mrp + 
               product_procurement_sla + Frequency + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, data = train_Game_mult[,])
summary(model_5) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_5))

#removing Frequency with high vif and low significance
model_6 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               sla + product_mrp + 
               product_procurement_sla + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + 
               product_analytic_verticalGamingMouse + product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, data = train_Game_mult[,])
summary(model_6) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_6))

#removing product_analytic_verticalGamingMouse with low significance and high VIF
model_7 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               sla + product_mrp + 
               product_procurement_sla + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + 
               product_analytic_verticalGamingMousePad + 
               product_analytic_verticalMotionController, data = train_Game_mult[,])
summary(model_7) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_7))

#removing product_analytic_verticalMotionController with low significance
model_8 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               sla + product_mrp + 
               product_procurement_sla + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + 
               product_analytic_verticalGamingMousePad , data = train_Game_mult[,])
summary(model_8) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_8))

#removing product_mrp with low significance
model_9 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
               sla + 
               product_procurement_sla + NPS_Score + special_sales + 
               TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
               OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
               weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
               product_analytic_verticalGamePad + 
               product_analytic_verticalGamingMousePad , data = train_Game_mult[,])
summary(model_9) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718  
sort(vif(model_9))

#removing gmv with low significance
model_10 <-lm(formula = weekly_gmv ~ Month + Year + deliverybdays + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamePad + 
                product_analytic_verticalGamingMousePad , data = train_Game_mult[,])
summary(model_10) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_10))

#removing deliverybdays with high VIF and low significance
model_11 <-lm(formula = weekly_gmv ~ Month + Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamePad + 
                product_analytic_verticalGamingMousePad , data = train_Game_mult[,])
summary(model_11) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_11))

#removing product_analytic_verticalGamePad with low significance
model_12 <-lm(formula = weekly_gmv ~ Month + Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount + 
                product_analytic_verticalGamingMousePad , data = train_Game_mult[,])
summary(model_12) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9718 
sort(vif(model_12))

#removing product_analytic_verticalGamingMousePad with low significance
model_13 <-lm(formula = weekly_gmv ~ Month + Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                OnlineMarketing + Affiliates + SEM + Radio + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount , data = train_Game_mult[,])
summary(model_13) #Multiple R-squared:  0.9718,	Adjusted R-squared:  0.9717 
sort(vif(model_13))

#removing OnlineMarketing with high VIF
model_14 <-lm(formula = weekly_gmv ~ Month + Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                Affiliates + SEM + Radio + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount , data = train_Game_mult[,])
summary(model_14) #Multiple R-squared:  0.9717,	Adjusted R-squared:  0.9717 
sort(vif(model_14))

#removing Radio with high VIF
model_15 <-lm(formula = weekly_gmv ~ Month + Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TotalInvestment + TV + Digital + Sponsorship + ContentMarketing + 
                Affiliates + SEM + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount , data = train_Game_mult[,])
summary(model_15) #Multiple R-squared:  0.9713,	Adjusted R-squared:  0.9713 
sort(vif(model_15))

#removing TotalInvestment with high VIF
model_16 <-lm(formula = weekly_gmv ~ Month + Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + ContentMarketing + 
                Affiliates + SEM + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount , data = train_Game_mult[,])
summary(model_16) #Multiple R-squared:  0.9711,	Adjusted R-squared:  0.9711 
sort(vif(model_16))

#removing ContentMarketing with relatively high VIF and shows less impact on r squared
model_17 <-lm(formula = weekly_gmv ~ Month + Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount , data = train_Game_mult[,])
summary(model_17) #Multiple R-squared:  0.9711,	Adjusted R-squared:  0.971 
sort(vif(model_17))

#removing Month with relatively high VIF and shows less impact on r squared
model_18 <-lm(formula = weekly_gmv ~ Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount , data = train_Game_mult[,])
summary(model_18) #Multiple R-squared:  0.9643,	Adjusted R-squared:  0.9643
sort(vif(model_18))

#removing SEM with high VIF
model_19 <-lm(formula = weekly_gmv ~ Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + discountpercentage + 
                weekly_sold_units + highrevenue + highinvestment + weekly_avg_discount , data = train_Game_mult[,])
summary(model_19) #Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9613 
sort(vif(model_19))

#removing highinvestment with relatively high VIF and shows less impact on r squared
model_20 <-lm(formula = weekly_gmv ~ Year + sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + discountpercentage + 
                weekly_sold_units + highrevenue + weekly_avg_discount , data = train_Game_mult[,])
summary(model_20) #Multiple R-squared:  0.9589,	Adjusted R-squared:  0.9589
sort(vif(model_20))

#removing Year with relatively high VIF and shows less impact on r squared
model_21 <-lm(formula = weekly_gmv ~ sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + discountpercentage + 
                weekly_sold_units + highrevenue + weekly_avg_discount , data = train_Game_mult[,])
summary(model_21) #Multiple R-squared:  0.9574,	Adjusted R-squared:  0.9574  
sort(vif(model_21))

#removing discountpercentage with less significance
model_22 <-lm(formula = weekly_gmv ~ sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + 
                weekly_sold_units + highrevenue + weekly_avg_discount , data = train_Game_mult[,])
summary(model_22) #Multiple R-squared:  0.9574,	Adjusted R-squared:  0.9574  
sort(vif(model_22))

#removing Affiliates with high VIF
Mult_Game_model_23 <-lm(formula = weekly_gmv ~ sla + 
                product_procurement_sla + NPS_Score + special_sales + 
                TV + Digital + Sponsorship + 
                Other + 
                weekly_sold_units + highrevenue + weekly_avg_discount , data = train_Game_mult[,])
summary(Mult_Game_model_23) #Multiple R-squared:  0.9435,	Adjusted R-squared:  0.9435  
sort(vif(Mult_Game_model_23))
#All these 11 variables are significant with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- Mult_Game_model_23
train_data <- train_Game_mult
test_data <- test_Game_mult
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2      RMSE       MAE       MAPE
# 0.9446091 0.1243484 0.1019738 0.05738583

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.061 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###KOYCK Model - to see the carry over effect#################

str(GamingAccessory_Model_1) #175478 obs. of  44 variables

#creating lag of n = 1 for the dependant variable - weekly_gmv
GamingAccessory_Koyck <- GamingAccessory_Model_1
GamingAccessory_Koyck$weekly_gmv_lag1<-lag(GamingAccessory_Koyck$weekly_gmv,n=1)
str(GamingAccessory_Koyck) #175478 obs. of  45 variables

#look for NA and omit
sum(is.na(GamingAccessory_Koyck)) #1 - first row
GamingAccessory_Koyck <- na.omit(GamingAccessory_Koyck)
sum(is.na(GamingAccessory_Koyck)) #0 NAs

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_Koyck), 0.7*nrow(GamingAccessory_Koyck))

#test and train data
train_gameacc_koyck=GamingAccessory_Koyck[indices,]
test_gameacc_koyck = GamingAccessory_Koyck[-indices,]

#model 1
k_model_1 <- lm(weekly_gmv~.,data=train_gameacc_koyck)
summary(k_model_1)
#Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
#3 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(k_model_1, direction="both")

# Run the step object
step

k_model_2 <- lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
                  deliverycdays + s1_fact.order_payment_type + product_mrp + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                  highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_2) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996 
sort(vif(k_model_2))

#removing deliverycdays with low significance 
k_model_3 <-lm(formula = weekly_gmv ~ Month + Year + gmv + deliverybdays + 
                 s1_fact.order_payment_type + product_mrp + 
                 product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                 TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                 Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                 highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_3) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_3))

#removing deliverybdays with low significance
k_model_4 <- lm(formula = weekly_gmv ~ Month + Year + gmv + 
                  s1_fact.order_payment_type + product_mrp + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                  highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_4) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_4))

#removing s1_fact.order_payment_type with low significance and high VIF
k_model_5 <- lm(formula = weekly_gmv ~ Month + Year + gmv + 
                  product_mrp + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                  highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_5) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_5))

#removing gmv with low significance
k_model_6 <- lm(formula = weekly_gmv ~ Month + Year + product_mrp + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                  highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_6) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_6))

#removing product_procurement_sla with low significance
k_model_7 <- lm(formula = weekly_gmv ~ Month + Year + 
                  product_mrp + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                  highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_7) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_7))

#removing TotalInvestment with high VIF
k_model_8 <- lm(formula = weekly_gmv ~ Month + Year + 
                  product_mrp + NPS_Score + special_sales  + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                  highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_8) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_8))

#removing TV with low significance and relatively high VIF
k_model_9 <- lm(formula = weekly_gmv ~ Month + Year + 
                  product_mrp + NPS_Score + special_sales  + 
                  Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                  highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_9) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_9))

#removing ContentMarketing with low significance
k_model_10 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + NPS_Score + special_sales  + 
                   Digital + Sponsorship + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_10) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_10))

#removing Sponsorship with low significance  
k_model_11 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + NPS_Score + special_sales  + 
                   Digital + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_11) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_11))

#removing SEM with high VIF
k_model_12 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + NPS_Score + special_sales  + 
                   Digital + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_12) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_12))

#removing Other with low significance
k_model_13 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + NPS_Score + special_sales  + 
                   Digital + OnlineMarketing + 
                   Affiliates + Radio + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_13) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_13))

#removing highinvestment with low significance
k_model_14 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + NPS_Score + special_sales  + 
                   Digital + OnlineMarketing + 
                   Affiliates + Radio + weekly_sold_units + highrevenue + 
                   weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_14) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_14))

#Removing Digital with low significance 
k_model_15 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + NPS_Score + special_sales  + 
                   OnlineMarketing + 
                   Affiliates + Radio + weekly_sold_units + highrevenue + 
                   weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_15) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_15))

#removing OnlineMarketing with high VIF
k_model_16 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + NPS_Score + special_sales  + 
                   Affiliates + Radio + weekly_sold_units + highrevenue + 
                   weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_16) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_16))

#removing NPS_Score with low significance
k_model_17 <- lm(formula = weekly_gmv ~ Month + Year + 
                   product_mrp + special_sales  + 
                   Affiliates + Radio + weekly_sold_units + highrevenue + 
                   weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_17) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_17))

#removing weekly_sold_units with relatively high VIF and shows no impact on R Squared
k_model_18 <-  lm(formula = weekly_gmv ~ Month + Year + 
                    product_mrp + special_sales  + 
                    Affiliates + Radio + highrevenue + 
                    weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_18) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_18))

#removing Month with low significance
k_model_19 <- lm(formula = weekly_gmv ~ Year + 
                   product_mrp + special_sales  + 
                   Affiliates + Radio + highrevenue + 
                   weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_model_19) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_model_19))

#removing Radio with low significance
k_game_model_20 <- lm(formula = weekly_gmv ~ Year + 
                   product_mrp + special_sales  + 
                   Affiliates + highrevenue + 
                   weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_koyck)
summary(k_game_model_20) #Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9996  
sort(vif(k_game_model_20))
#All these 7 variables are significant(p<0.05) with VIF less than 5

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- k_game_model_20
train_data <- train_gameacc_koyck
test_data <- test_gameacc_koyck
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2      RMSE         MAE         MAPE
# 0.9998926 0.05970407 0.003547234 0.001250546

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.006 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###Distributed lag - to capture the carry-over effect#################

str(GamingAccessory_Model_1) #175478 obs. of  44 variables

#creating lag of n = 1, 2 and 3 for the dependant variable
GameAccessory_Dist <- GamingAccessory_Model_1
for(i in 1:3){
  temp_var <- lag(GameAccessory_Dist$weekly_gmv,n=i)
  GameAccessory_Dist<-cbind(GameAccessory_Dist,temp_var)
  colnames(GameAccessory_Dist)[names(GameAccessory_Dist) == "temp_var"] <- paste("weekly_gmv_lag",i,sep="")
}
str(GameAccessory_Dist) #175478 obs. of  47 variables

#creating lag of n = 1, 2 for the independant investement variables
inv_vars <- c("TV","Digital","Sponsorship","ContentMarketing","OnlineMarketing","Affiliates","SEM","Radio","Other")
for(i in 1:2){
  for(j in inv_vars){
    temp_var <- lag(GameAccessory_Dist[,j],n=i)
    GameAccessory_Dist<-cbind(GameAccessory_Dist,temp_var)
    colnames(GameAccessory_Dist)[names(GameAccessory_Dist) == "temp_var"] <- paste(j,"_lag",i,sep="")
  }
}
str(GameAccessory_Dist) #175478 obs. of  65 variables

#look for NA and omit
sum(is.na(GameAccessory_Dist)) #33 - by introducing lag variables
GameAccessory_Dist <- na.omit(GameAccessory_Dist)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(GameAccessory_Dist), 0.7*nrow(GameAccessory_Dist))

#test and train data
train_gameacc_dist = GameAccessory_Dist[indices,]
test_gameacc_dist = GameAccessory_Dist[-indices,]

#model 1
D_model_1 <- lm(weekly_gmv~.,data=train_gameacc_dist)
summary(D_model_1)
#Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998 
#3 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(D_model_1, direction="both")

# Run the step object
step

D_model_2 <- lm(formula = weekly_gmv ~ Month + Year + s1_fact.order_payment_type + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_2) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998 
sort(vif(D_model_2))

#removing Year as it has low significance and relatively high VIF
D_model_3 <- lm(formula = weekly_gmv ~ Month + s1_fact.order_payment_type + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + discountpercentage + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_3) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_3))

#removing discountpercentage as it has low significance and relatively high VIF
D_model_4 <- lm(formula = weekly_gmv ~ Month + s1_fact.order_payment_type + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_4) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_4))

#removing Month as it has low significance and relatively high VIF 
D_model_5 <- lm(formula = weekly_gmv ~ s1_fact.order_payment_type + 
                  product_procurement_sla + NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_5) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_5))

#removing product_procurement_sla due to low significance
D_model_6 <- lm(formula = weekly_gmv ~ s1_fact.order_payment_type + 
                  NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_6) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_6))

#removing Sponsorship_lag1 due to low significance  and high VIF
D_model_7 <- lm(formula = weekly_gmv ~ s1_fact.order_payment_type + 
                  NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_7) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_7))

#removing s1_fact.order_payment_type due to low significance  
D_model_8 <- lm(formula = weekly_gmv ~  
                  NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + SEM + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_8) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_8))

#removing SEM due to high VIF
D_model_9 <- lm(formula = weekly_gmv ~  
                  NPS_Score + special_sales + TotalInvestment + 
                  TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                  Affiliates + Radio + Other + weekly_sold_units + 
                  highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                  TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                  OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                  Other_lag1, data = train_gameacc_dist)
summary(D_model_9) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_9))

#removing Radio_lag1 due to low significance and high VIF
D_model_10 <- lm(formula = weekly_gmv ~  
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Radio + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_10) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_10))

#removing Radio due to low significance
D_model_11 <- lm(formula = weekly_gmv ~  
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_11) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_11))

#removing OnlineMarketing due to high VIF
D_model_12 <- lm(formula = weekly_gmv ~  
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + 
                   Affiliates + Other + weekly_sold_units + 
                   highrevenue + highinvestment + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_12) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_12))

#removing highinvestment due to low significance
D_model_13 <- lm(formula = weekly_gmv ~  
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + 
                   Affiliates + Other + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_13) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_13))

#removing TotalInvestment due to high VIF
D_model_14 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + 
                   Affiliates + Other + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + Digital_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_14) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_14))

#removing Digital_lag1 due to low significance and high VIF
D_model_15 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                   TV + Digital + Sponsorship + ContentMarketing + 
                   Affiliates + Other + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_15) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_15))

#removing Sponsorship due to low significance
D_model_16 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                   TV + Digital + ContentMarketing + 
                   Affiliates + Other + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_16) #Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9998
sort(vif(D_model_16))

#removing Other due to high VIF
D_model_17 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                   TV + Digital + ContentMarketing + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_17) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_17))

#removing Affiliates_lag1 due to high VIF
D_model_18 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + 
                   TV + Digital + ContentMarketing + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1  + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_18) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_18))

#removing ContentMarketing due to low significance
D_model_19 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + TV + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   TV_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1  + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_19) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_19))

#removing ContentMarketing_lag1 due to high VIF and low significance
D_model_20 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + TV + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 +   TV_lag1 + 
                   OnlineMarketing_lag1  + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_20) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_20))

#removing TV_lag1 due to high VIF and low significance
D_model_21 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + TV + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   OnlineMarketing_lag1  + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_21) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_21))

#removing TV due to low significance
D_model_22 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   OnlineMarketing_lag1  + SEM_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_22) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_22))

#removing SEM_lag1 due to high VIF
D_model_23 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   OnlineMarketing_lag1 + 
                   Other_lag1, data = train_gameacc_dist)
summary(D_model_23) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_23))

#removing Other_lag1 due to low significance
D_model_24 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1 + 
                   OnlineMarketing_lag1, data = train_gameacc_dist)
summary(D_model_24) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_24))

#removing OnlineMarketing_lag1 due to high VIF
D_model_25 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_avg_discount + weekly_gmv_lag1, data = train_gameacc_dist)
summary(D_model_25) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_25))

#removing weekly_avg_discount due to low significance
D_model_26 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + Digital + 
                   Affiliates + weekly_sold_units + 
                   highrevenue + weekly_gmv_lag1, data = train_gameacc_dist)
summary(D_model_26) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_26))

#weekly_gmv_lag1 VIF = 28.68
#weekly_sold_units VIF = 24.74
#removing weekly_sold_units instead of weekly_gmv_lag1 as it is not impacting the R2
D_model_27 <- lm(formula = weekly_gmv ~ NPS_Score + special_sales + Digital + 
                   Affiliates + weekly_gmv_lag1 + 
                   highrevenue, data = train_gameacc_dist)
summary(D_model_27) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_27))

#removing NPS_Score due to low significance
D_model_28 <- lm(formula = weekly_gmv ~ special_sales + Digital + 
                   Affiliates + weekly_gmv_lag1 + 
                   highrevenue, data = train_gameacc_dist)
summary(D_model_28) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_model_28))

#removing Affiliates due to low significance
D_game_model_29 <- lm(formula = weekly_gmv ~ special_sales + Digital + weekly_gmv_lag1 + 
                        highrevenue, data = train_gameacc_dist)
summary(D_game_model_29) #Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9997
sort(vif(D_game_model_29))

#All these 4 variables are significant with VIF less than 5
#Although the Digital variable has less significance, we are retaining Digital 
#since the p value is less than 0.05 and the varibale is relevant to case study

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- D_game_model_29
train_data <- train_gameacc_dist
test_data <- test_gameacc_dist
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2       RMSE         MAE        MAPE
#  0.9996056 0.1143274 0.002510257 0.0005356987

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.0129 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

###multiplicative Distributed lag - to capture both interactive & carry-over effect#################

str(GamingAccessory_Model_1) #175478 obs. of  44 variables

#creating lag of n = 1, 2 and 3 for the dependant variable
GamingAccessory_MultDist <- GamingAccessory_Model_1
for(i in 1:3){
  temp_var <- lag(GamingAccessory_MultDist$weekly_gmv,n=i)
  GamingAccessory_MultDist<-cbind(GamingAccessory_MultDist,temp_var)
  colnames(GamingAccessory_MultDist)[names(GamingAccessory_MultDist) == "temp_var"] <- paste("weekly_gmv_lag",i,sep="")
}
str(GamingAccessory_MultDist) #175478 obs. of  47 variables

#creating lag of n = 1, 2 for the independant investement variables
inv_vars <- c("TV","Digital","Sponsorship","ContentMarketing","OnlineMarketing","Affiliates","SEM","Radio","Other")
for(i in 1:2){
  for(j in inv_vars){
    temp_var <- lag(GamingAccessory_MultDist[,j],n=i)
    GamingAccessory_MultDist<-cbind(GamingAccessory_MultDist,temp_var)
    colnames(GamingAccessory_MultDist)[names(GamingAccessory_MultDist) == "temp_var"] <- paste(j,"_lag",i,sep="")
  }
}
str(GamingAccessory_MultDist) #175478 obs. of  65 variables

#look for NA and omit
sum(is.na(GamingAccessory_MultDist)) #33 - by introducing lag variables
GamingAccessory_MultDist <- na.omit(GamingAccessory_MultDist)

# Divide you data in 70:30 
set.seed(100)
indices= sample(1:nrow(GamingAccessory_MultDist), 0.7*nrow(GamingAccessory_MultDist))

#test and train data
train_game_multdist=GamingAccessory_MultDist[indices,]
test_game_multdist = GamingAccessory_MultDist[-indices,]

#to avoid NAs while taking log values
train_game_multdist[train_game_multdist == 0] <- 0.00001
test_game_multdist[test_game_multdist == 0] <- 0.00001

#log of values
train_game_multdist <- log(train_game_multdist)
test_game_multdist <- log(test_game_multdist)

sum(is.na(train_game_multdist))# no missing values
sum(is.na(test_game_multdist))# no missing values

#model 1
MD_model_1 <- lm(weekly_gmv~.,data=train_game_multdist)
summary(MD_model_1)
#Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
#3 not defined because of singularities

# Apply the stepwise approach
step <- stepAIC(MD_model_1, direction="both")

# Run the step object
step

MD_model_2 <- lm(formula = weekly_gmv ~ Month + Year + gmv + product_procurement_sla + 
                   Frequency + NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                   weekly_gmv_lag1 + weekly_gmv_lag2 + weekly_gmv_lag3 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1 + SEM_lag2, data = train_game_multdist)
summary(MD_model_2) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995 
sort(vif(MD_model_2))

#removing weekly_gmv_lag2 as it has less significance
MD_model_3 <- lm(formula = weekly_gmv ~ Month + Year + gmv + product_procurement_sla + 
                   Frequency + NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount + product_analytic_verticalGamePad + 
                   weekly_gmv_lag1  + weekly_gmv_lag3 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1 + SEM_lag2, data = train_game_multdist)
summary(MD_model_3) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995
sort(vif(MD_model_3))

#removing product_analytic_verticalGamePad as it has less significance and relatively high VIF
MD_model_4 <- lm(formula = weekly_gmv ~ Month + Year + gmv + product_procurement_sla + 
                   Frequency + NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount  + 
                   weekly_gmv_lag1  + weekly_gmv_lag3 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1 + SEM_lag2, data = train_game_multdist)
summary(MD_model_4) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995
sort(vif(MD_model_4))

#removing Frequency as it has less significance
MD_model_5 <- lm(formula = weekly_gmv ~ Month + Year + gmv + product_procurement_sla + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount  + 
                   weekly_gmv_lag1  + weekly_gmv_lag3 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1 + SEM_lag2, data = train_game_multdist)
summary(MD_model_5) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995
sort(vif(MD_model_5))

#remove gmv as it has less significance
MD_model_6 <- lm(formula = weekly_gmv ~ Month + Year + product_procurement_sla + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount  + 
                   weekly_gmv_lag1  + weekly_gmv_lag3 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1 + SEM_lag2, data = train_game_multdist)
summary(MD_model_6) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995
sort(vif(MD_model_6))

#remove SEM_lag2 as it has less significance and high VIF
MD_model_7 <- lm(formula = weekly_gmv ~ Month + Year + product_procurement_sla + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount  + 
                   weekly_gmv_lag1  + weekly_gmv_lag3 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_game_multdist)
summary(MD_model_7) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995
sort(vif(MD_model_7))

#remove weekly_gmv_lag3 as it is less significant
MD_model_8 <- lm(formula = weekly_gmv ~ Month + Year + product_procurement_sla + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount  + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_game_multdist)
summary(MD_model_8) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995
sort(vif(MD_model_8))

#removing product_procurement_sla as it is less significant
MD_model_9 <- lm(formula = weekly_gmv ~ Month + Year + 
                   NPS_Score + special_sales + TotalInvestment + 
                   TV + Digital + Sponsorship + ContentMarketing + OnlineMarketing + 
                   Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                   highinvestment + weekly_avg_discount  + 
                   weekly_gmv_lag1 + TV_lag1 + 
                   Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                   OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                   Other_lag1, data = train_game_multdist)
summary(MD_model_9) #Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9995
sort(vif(MD_model_9))

#removing OnlineMarketing with high VIF
MD_model_10 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + Sponsorship + ContentMarketing + 
                    Affiliates + SEM + Radio + Other + weekly_sold_units + highrevenue + 
                    highinvestment + weekly_avg_discount  + 
                    weekly_gmv_lag1 + TV_lag1 + 
                    Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                    OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                    Other_lag1, data = train_game_multdist)
summary(MD_model_10) #Multiple R-squared:  0.9994,	Adjusted R-squared:  0.9994
sort(vif(MD_model_10))

#removing Radio with high VIF
MD_model_11 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + Sponsorship + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    highinvestment + weekly_avg_discount  + 
                    weekly_gmv_lag1 + TV_lag1 + 
                    Digital_lag1 + Sponsorship_lag1 + ContentMarketing_lag1 + 
                    OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                    Other_lag1, data = train_game_multdist)
summary(MD_model_11) #Multiple R-squared:  0.9994,	Adjusted R-squared:  0.9994
sort(vif(MD_model_11))

#removing Digital_lag1 with high VIF
MD_model_12 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + Sponsorship + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    highinvestment + weekly_avg_discount  + 
                    weekly_gmv_lag1 + TV_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                    Other_lag1, data = train_game_multdist)
summary(MD_model_12) #Multiple R-squared:  0.9994,	Adjusted R-squared:  0.9994
sort(vif(MD_model_12))

#removing Sponsorship with high VIF
MD_model_13 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    highinvestment + weekly_avg_discount  + 
                    weekly_gmv_lag1 + TV_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    OnlineMarketing_lag1 + Affiliates_lag1 + SEM_lag1 + Radio_lag1 + 
                    Other_lag1, data = train_game_multdist)
summary(MD_model_13) #Multiple R-squared:  0.9994,	Adjusted R-squared:  0.9994
sort(vif(MD_model_13))

#removing Affiliates_lag1 with high VIF
MD_model_14 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    highinvestment + weekly_avg_discount  + 
                    weekly_gmv_lag1 + TV_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    OnlineMarketing_lag1 + SEM_lag1 + Radio_lag1 + 
                    Other_lag1, data = train_game_multdist)
summary(MD_model_14) #Multiple R-squared:  0.9993,	Adjusted R-squared:  0.9993 
sort(vif(MD_model_14))

#removing highinvestment with low significance
MD_model_15 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + 
                    weekly_gmv_lag1 + TV_lag1 + 
                    Sponsorship_lag1 + ContentMarketing_lag1 + 
                    OnlineMarketing_lag1 + SEM_lag1 + Radio_lag1 + 
                    Other_lag1, data = train_game_multdist)
summary(MD_model_15) #Multiple R-squared:  0.9993,	Adjusted R-squared:  0.9993 
sort(vif(MD_model_15))

#removing ContentMarketing_lag1 with high VIF
MD_model_16 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 + 
                    OnlineMarketing_lag1 + SEM_lag1 + Radio_lag1 + 
                    Other_lag1, data = train_game_multdist)
summary(MD_model_16) #Multiple R-squared:  0.9993,	Adjusted R-squared:  0.9993 
sort(vif(MD_model_16))

#removing Other_lag1 with high VIF
MD_model_17 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    TV + Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 + 
                    OnlineMarketing_lag1 + SEM_lag1 + Radio_lag1
                  , data = train_game_multdist)
summary(MD_model_17) #Multiple R-squared:  0.9992,	Adjusted R-squared:  0.9992
sort(vif(MD_model_17))

#removing TV with high VIF
MD_model_18 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 + 
                    OnlineMarketing_lag1 + SEM_lag1 + Radio_lag1
                  , data = train_game_multdist)
summary(MD_model_18) #Multiple R-squared:  0.9987,	Adjusted R-squared:  0.9987 
sort(vif(MD_model_18))

#removing Radio_lag1 due to low significance
MD_model_19 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 + 
                    OnlineMarketing_lag1 + SEM_lag1
                  , data = train_game_multdist)
summary(MD_model_19) #Multiple R-squared:  0.9987,	Adjusted R-squared:  0.9987 
sort(vif(MD_model_19))

#removing SEM_lag1 due to low significance
MD_model_20 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 + 
                    OnlineMarketing_lag1
                  , data = train_game_multdist)
summary(MD_model_20) #Multiple R-squared:  0.9987,	Adjusted R-squared:  0.9987 
sort(vif(MD_model_20))

#removing OnlineMarketing_lag1 due to high VIF 
MD_model_21 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales + TotalInvestment + 
                    Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 
                  , data = train_game_multdist)
summary(MD_model_21) #Multiple R-squared:  0.9969,	Adjusted R-squared:  0.9969 
sort(vif(MD_model_21))

#removing TotalInvestment due to high VIF 
MD_model_22 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    Affiliates + SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 
                  , data = train_game_multdist)
summary(MD_model_22) #Multiple R-squared:  0.9968,	Adjusted R-squared:  0.9968 
sort(vif(MD_model_22))

#removing Affiliates due to high VIF 
MD_model_23 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 +  Sponsorship_lag1 
                  , data = train_game_multdist)
summary(MD_model_23) #Multiple R-squared:  0.9968,	Adjusted R-squared:  0.9968 
sort(vif(MD_model_23))

#removing Sponsorship_lag1 due to low singificance
MD_model_24 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    SEM + Other + weekly_sold_units + highrevenue + 
                    weekly_avg_discount  + weekly_gmv_lag1 + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_24) #Multiple R-squared:  0.9968,	Adjusted R-squared:  0.9968 
sort(vif(MD_model_24))

#weekly_sold_units VIF = 17.43
#weekly_gmv_lag1 VIF = 24.72
#removing weekly_sold_units as removing weekly_gmv_lag1 is reducing the R2
MD_model_25 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    SEM + Other + weekly_gmv_lag1 + highrevenue + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_25) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_25))

#removing SEM due to high VIF and low singificance
MD_model_26 <- lm(formula = weekly_gmv ~ Month + Year + 
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    Other + weekly_gmv_lag1 + highrevenue + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_26) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_26))

#removing Month due to low significance and high VIF
MD_model_27 <- lm(formula = weekly_gmv ~ Year + 
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    Other + weekly_gmv_lag1 + highrevenue + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_27) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_27))

#removing Year due to low significance and high VIF
MD_model_28 <- lm(formula = weekly_gmv ~  
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    Other + weekly_gmv_lag1 + highrevenue + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_28) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_28))

#removing highrevenue due to low significance and high VIF
MD_model_29 <- lm(formula = weekly_gmv ~  
                    NPS_Score + special_sales  + 
                    Digital + ContentMarketing + 
                    Other + weekly_gmv_lag1 + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_29) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_29))

#removing Digital due to low significance and high VIF
MD_model_30 <- lm(formula = weekly_gmv ~  
                    NPS_Score + special_sales  + 
                    ContentMarketing + 
                    Other + weekly_gmv_lag1 + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_30) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_30))

#removing special_sales due to high VIF and low signficance
MD_model_31 <- lm(formula = weekly_gmv ~  
                    NPS_Score  + 
                    ContentMarketing + 
                    Other + weekly_gmv_lag1 + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_31) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_31))

#removing Other due to low signficance
MD_model_32 <- lm(formula = weekly_gmv ~  
                    NPS_Score  + 
                    ContentMarketing + 
                    weekly_gmv_lag1 + 
                    weekly_avg_discount  + TV_lag1 
                  , data = train_game_multdist)
summary(MD_model_32) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_32))

#removing TV_lag1 due to low signficance
MD_model_33 <- lm(formula = weekly_gmv ~  
                    NPS_Score  + 
                    ContentMarketing + 
                    weekly_gmv_lag1 + 
                    weekly_avg_discount  
                  , data = train_game_multdist)
summary(MD_model_33) #Multiple R-squared:  0.9966,	Adjusted R-squared:  0.9966 
sort(vif(MD_model_33))
#All these 4 variables are significant with VIF less than 5.
#ContentMarketing has VIF of ~6 but retaining it as it is relavant for our study

#Evaluating the model performance using Validation set Approach

#setting neccesary variables
final_model <- MD_model_33
train_data <- train_game_multdist
test_data <- test_game_multdist
num_row <- length(final_model$coefficients)

#for accuracy, stability, generalisability/overfitting
# Make predictions and compute the R2, RMSE and MAE
predictions <- final_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$weekly_gmv),
            RMSE = RMSE(predictions, test_data$weekly_gmv),
            MAE = MAE(predictions, test_data$weekly_gmv),
            MAPE = MAPE(predictions,test_data$weekly_gmv))
#        R2        RMSE       MAE        MAPE
#  0.995308 0.03610621 0.0007583383 0.0005175812

#Prediction Error Rate - RSME divided by average value of dependant/outcome variable
RMSE(predictions, test_data$weekly_gmv)/mean(test_data$weekly_gmv)
#0.017 - shows low error rate

##Presenting our findings - Visualisation - Elasticity of each KPIs
elasticity_values <- setNames(data.frame(matrix(ncol = 2, nrow = num_row-1)), c("KPI", "Elasticity"))
elasticity_values$KPI<-names(final_model$coefficients)[2:num_row]
for(i in 1:num_row-1){
  elasticity_values[i,2] <-elasticity_fun(elasticity_values[i,1])
}
elasticity_values

#########################################################################################################

####Please see the documentation for the recommendations based on the above analysis#####################

