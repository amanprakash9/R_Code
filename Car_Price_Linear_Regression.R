#Setting up working directory
setwd("C:/Users/HP/Downloads")

# Load essential libraries
install.packages("MASS")
install.packages("car")
library(ggplot2)
library(lubridate)
library(stringr)
library(MASS)
library(car)

install.packages("MASS")

# load the carprice company data
carprice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)
str(carprice)

#vieiwing the dataset
View(carprice)

#Check for duplicate rows in dataframe.
unique(carprice)

View(carprice)
#the total no. of observations in the dataset is still 205 which mean
#no duplicates in the data set

#Check for missing values
sum(is.na(carprice))

#We get 0 missing values,hence no missing rows in dataset which needs to be treated.

#Removing the first column which gives unique number to the each row.
carprice <- carprice[,-1]

#Deriving a new column CarCompany as it will be used to determine carprice.
carprice$CarCompany <-  word(carprice$CarName,1)

#Removing the column CarName as it will not be a factor to determing carprice.
carprice <-  carprice[,-2]

str(carprice)


#DUMMY VARIABLE CREATION. 
#Let us see the structure of variable "fueltype".
str(carprice$fueltype)
summary(factor(carprice$fueltype))

#convert mainroad variable to numeric is to replace the levels- Gas and Diesel with 1 and 0 is:

carprice$fueltype[which(carprice$fueltype=="gas")] <- "1"
carprice$fueltype[which(carprice$fueltype=="diesel")] <- "0"

# Now store the numeric values in the same variable
carprice$fueltype<- as.numeric(carprice$fueltype)

# Check the summary of mainroad variable
summary(carprice$fueltype)



# Do the same for other such categorical variables

carprice$aspiration[which(carprice$aspiration=="std")] <- "1"
carprice$aspiration[which(carprice$aspiration=="turbo")] <- "0"
carprice$aspiration<- as.numeric(carprice$aspiration)


carprice$doornumber[which(carprice$doornumber=="two")] <- "1"
carprice$doornumber[which(carprice$doornumber=="four")] <- "0"
carprice$doornumber<- as.numeric(carprice$doornumber)


carprice$enginelocation[which(carprice$enginelocation=="front")] <- "1"
carprice$enginelocation[which(carprice$enginelocation=="rear")] <- "0"
carprice$enginelocation<- as.numeric(carprice$enginelocation)


#creating dummy varaibles for different categories of carbody.
#carbody dummy variables
dummy_1 <- data.frame(model.matrix( ~carbody, data = carprice))

#The newly created dummy_1 dataframe X intercept column needs to be removed
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables to the main data set, after removing the original column
carprice <- cbind(carprice[,-5], dummy_1)


#creating dummy varaibles for different categories of enginetype. 
dummy_2 <- data.frame(model.matrix( ~enginetype, data = carprice))

#The newly created dummy_2 dataframe X intercept column needs to be removed
dummy_2 <- dummy_2[,-1]

# Combine the dummy variables to the main data set, after removing the original column
carprice <- cbind(carprice[,-12], dummy_2)


#creating dummy varaibles for different categories of cylindernumber. 
dummy_3 <- data.frame(model.matrix( ~cylindernumber, data = carprice))

#The newly created dummy_1 dataframe X intercept column needs to be removed
dummy_3 <- dummy_3[,-1]

# Combine the dummy variables to the main data set, after removing the original column
carprice <- cbind(carprice[,-12], dummy_3)


#creating dummy varaibles for different categories of fuelsystem. 
dummy_4 <- data.frame(model.matrix( ~fuelsystem, data = carprice))

#The newly created dummy_1 dataframe X intercept column needs to be removed
dummy_4 <- dummy_4[,-1]

# Combine the dummy variables to the main data set, after removing the original column
carprice <- cbind(carprice[,-13], dummy_4)



#creating dummy varaibles for different categories of CarCompany. 
dummy_5 <- data.frame(model.matrix( ~CarCompany, data = carprice))

#The newly created dummy_1 dataframe X intercept column needs to be removed
dummy_5 <- dummy_5[,-1]

# Combine the dummy variables to the main data set, after removing the original column
carprice <- cbind(carprice[,-21], dummy_5)

#converting carprice into character for dummy variable creation.
str(carprice)
carprice$symboling<- as.character(carprice$symboling)


#creating dummy varaibles for different categories of symboling. 
dummy_6 <- data.frame(model.matrix( ~symboling, data = carprice))

#The newly created dummy_1 dataframe X intercept column needs to be removed
dummy_6 <- dummy_6[,-1]

# Combine the dummy variables to the main data set, after removing the original column
carprice <- cbind(carprice[,-1], dummy_6)


#creating dummy varaibles for different categories of drivewheel. 
dummy_7 <- data.frame(model.matrix( ~drivewheel, data = carprice))

#The newly created dummy_1 dataframe X intercept column needs to be removed
dummy_7 <- dummy_7[,-1]

# Combine the dummy variables to the main data set, after removing the original column
carprice <- cbind(carprice[,-4], dummy_7)


#removing outliers from the numeric variables-:

quantile(carprice$wheelbase, seq(0,1,0.01))#No outliers
quantile(carprice$carlength, seq(0,1,0.01))#No outliers
quantile(carprice$carwidth, seq(0,1,0.01))#No outliers
quantile(carprice$carheight, seq(0,1,0.01))#No outliers
quantile(carprice$curbweight, seq(0,1,0.01))#No outliers
quantile(carprice$enginesize, seq(0,1,0.01))#No outliers
quantile(carprice$boreratio, seq(0,1,0.01))#No outliers
quantile(carprice$stroke, seq(0,1,0.01))#No outliers

quantile(carprice$compressionratio, seq(0,1,0.01))
#The quantile is observed at 90,hence changing all the outlier values to 90th percentile value.
carprice$compressionratio[which(carprice$compressionratio>10.9400)]<-10.9400

quantile(carprice$horsepower, seq(0,1,0.01))
#The quantile is observed at 99,hence changing all the outlier values to 99th percentile value.
carprice$horsepower[which(carprice$horsepower>207.00)]<-207.00

quantile(carprice$peakrpm, seq(0,1,0.01))#No outliers
quantile(carprice$citympg, seq(0,1,0.01))#No outliers
quantile(carprice$highwaympg, seq(0,1,0.01))#No outliers



#Univarte Analysis on each parameter to determine the impact(EDA)

ggplot(carprice,aes(symboling.2))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(symboling0))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(symboling1))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(symboling3))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()


ggplot(carprice,aes(fueltype))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(aspiration))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(doornumber))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(carbodyhardtop))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(carbodyhatchback))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(carbodysedan))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(carbodywagon))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(drivewheelrwd))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(drivewheelfwd))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()


ggplot(carprice,aes(enginelocation))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(wheelbase))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(carlength))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(carwidth))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(carheight))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(curbweight))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginetypel))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginetypeohc))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginetypeohcf))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginetypedohcv))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginetyperotor))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginesize))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(boreratio))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginesize))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(enginelocation))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(stroke))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(compressionratio))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(horsepower))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(peakrpm))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(citympg))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()

ggplot(carprice,aes(highwaympg))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()


# We could find from that the variables enginesize,enginetyp, aspiration, stroke,compressionratio
#could be significant for price prediction as its major values are coming under specific area
#The variables having many dummy vairables can be derived from model creation.

#We could validate our EDA by model creation to find the factors effecting price/

#
# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))
# generate the train data set
train_carprice = carprice[trainindices,]

#Similarly store the rest of the observations into an object "test".
test_carprice = carprice[-trainindices,]


model_1<-lm(price~., data=train_carprice)

summary(model_1)


#Using stepAIC to figure out the relevant columns for analysis.
step<-stepAIC(model_1, direction="both")

#The below step function will provide the column which should be used for next
#model.
step



model_2<-lm(price ~ aspiration  + enginelocation + 
              carwidth + curbweight + enginesize + stroke + compressionratio + 
              horsepower + peakrpm + citympg + carbodywagon + enginetypel + 
              enginetypeohc + enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              fuelsystemmpfi + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1 + drivewheelrwd, data = train_carprice)

summary(model_2)

vif(model_2)

# removing horsepower as it has highest vif with low significance

model_3<-lm(price ~ aspiration  + enginelocation + 
              carwidth + curbweight + enginesize + stroke + compressionratio + 
              peakrpm + citympg + carbodywagon + enginetypel + 
              enginetypeohc + enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              fuelsystemmpfi + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1 + drivewheelrwd, data = train_carprice)

summary(model_3)


vif(model_3)


cor(train_carprice$curbweight,train_carprice$enginesize)

#removing curb weight as it is highly correalted with enginesize and
#significance is less for curb wieght

model_4<-lm(price ~ aspiration  + enginelocation + 
              carwidth + enginesize + stroke + compressionratio + 
              peakrpm + citympg + carbodywagon + enginetypel + 
              enginetypeohc + enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              fuelsystemmpfi + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1 + drivewheelrwd, data = train_carprice)


summary(model_4)



vif(model_4)


# removing  citympg as it has high vif
model_5<-lm(price ~ aspiration + enginelocation + 
              carwidth + enginesize + stroke + compressionratio + 
              peakrpm + carbodywagon + enginetypel + 
              enginetypeohc + enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              fuelsystemmpfi + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1 + drivewheelrwd, data = train_carprice)


summary(model_5)

vif(model_5)



# removing  enginetypeohc high vif
model_6<-lm(price ~ aspiration  + enginelocation + 
              carwidth + enginesize + stroke + compressionratio + 
              peakrpm + carbodywagon + enginetypel + 
              enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              fuelsystemmpfi + CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1 + drivewheelrwd, data = train_carprice)


summary(model_6)



vif(model_6)

# removing fuelsystemmpfi high vif

model_7<-lm(price ~ aspiration + enginelocation + 
              carwidth + enginesize + stroke + compressionratio + 
              peakrpm + carbodywagon + enginetypel + 
              enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1 + drivewheelrwd, data = train_carprice)


summary(model_7)



vif(model_7)


cor(train_carprice$carwidth,train_carprice$enginesize)


#removing carwidtht as it is highly correalted with enginesize and
#significance is less for carwidth


model_8<-lm(price ~ aspiration + enginelocation + 
              enginesize + stroke + compressionratio + 
              peakrpm + carbodywagon + enginetypel + 
              enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1 + drivewheelrwd, data = train_carprice)


summary(model_8)



vif(model_8)



cor(train_carprice$drivewheelrwd,train_carprice$enginesize)

#removing drivewheelrwd as it is highly correalted with enginesize and high VIF

model_9<-lm(price ~ aspiration + enginelocation + 
              enginesize + stroke + compressionratio + 
              peakrpm + carbodywagon + enginetypel + 
              enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge + CarCompanyhonda + CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1, data = train_carprice)


summary(model_9)



vif(model_9)


#removing Companyhonda as it is high VIF.


model_10<-lm(price ~ aspiration + enginelocation + 
              enginesize + stroke + compressionratio + 
              peakrpm + carbodywagon + enginetypel + 
              enginetypeohcf + enginetypeohcv + enginetyperotor + 
              cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
              CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
              CarCompanydodge +  CarCompanyisuzu + CarCompanymaxda + 
              CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
              CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
              CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
              CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1, data = train_carprice)


summary(model_10)



vif(model_10)

# Removing cylindernumberfive as it has high VIF.

model_11<-lm(price ~ aspiration + enginelocation + 
               enginesize + stroke + compressionratio + 
               peakrpm + carbodywagon + enginetypel + 
               enginetypeohcf + enginetypeohcv + enginetyperotor + 
               cylindernumberthree + fuelsystem2bbl + 
               CarCompanyaudi + CarCompanybmw + CarCompanybuick + 
               CarCompanydodge +  CarCompanyisuzu + CarCompanymaxda + 
               CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
               CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
               CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
               CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1, data = train_carprice)


summary(model_11)



vif(model_11)

#Removing CarCompanybuick as it has high VIF.


model_12<-lm(price ~ aspiration + enginelocation + 
               enginesize + stroke + compressionratio + 
               peakrpm + carbodywagon + enginetypel + 
               enginetypeohcf + enginetypeohcv + enginetyperotor + 
               cylindernumberthree + fuelsystem2bbl + 
               CarCompanyaudi + CarCompanybmw + 
               CarCompanydodge +  CarCompanyisuzu + CarCompanymaxda + 
               CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
               CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
               CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
               CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1, data = train_carprice)


summary(model_12)

vif(model_12)

#Removing fuelsystem2bbl as it has high VIF.


model_13<-lm(price ~ aspiration + enginelocation + 
               enginesize + stroke + compressionratio + 
               peakrpm + carbodywagon + enginetypel + 
               enginetypeohcf + enginetypeohcv + enginetyperotor + 
               cylindernumberthree + 
               CarCompanyaudi + CarCompanybmw + 
               CarCompanydodge +  CarCompanyisuzu + CarCompanymaxda + 
               CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
               CarCompanyporcshce + CarCompanyrenault + CarCompanysaab + 
               CarCompanytoyota + CarCompanytoyouta + CarCompanyvokswagen + 
               CarCompanyvolkswagen + CarCompanyvolvo + CarCompanyvw + symboling1, data = train_carprice)


summary(model_13)



vif(model_13)

#We find that vif is less for all the vairables hence removing
#the variables based on its p value, removing all insignificant variables having p value greater then 0.09



model_14<-lm(price ~ aspiration + enginelocation + 
               enginesize + stroke + enginetypel + 
               enginetypeohcf + enginetyperotor + 
               CarCompanyaudi + CarCompanybmw + 
               CarCompanydodge +  CarCompanyisuzu + CarCompanymaxda + 
               CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
               CarCompanyrenault + 
               CarCompanytoyota + CarCompanytoyouta + 
               symboling1, data = train_carprice)


summary(model_14)

#removing varaible having p value greater then 0.05

model_15<-lm(price ~ aspiration + enginelocation + 
               enginesize + stroke + enginetypel + 
               enginetypeohcf + enginetyperotor + 
               CarCompanyaudi + CarCompanybmw + 
               CarCompanydodge +  CarCompanyisuzu + CarCompanymaxda + 
               CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanyNissan + CarCompanyplymouth + 
               CarCompanytoyota +symboling1, data = train_carprice)


summary(model_15)

# removing variables having p values greater then .009 that is (1 *)
model_16<-lm(price ~ aspiration + enginelocation + 
               enginesize + stroke + enginetypel + 
               enginetypeohcf + enginetyperotor + 
               CarCompanybmw + 
               CarCompanydodge +
               CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanyplymouth + 
               CarCompanytoyota, data = train_carprice)


summary(model_16)



# removing variables having p values greater then .0009 that is (2 *)
model_17<-lm(price ~ aspiration + enginelocation + 
               enginesize + stroke + 
               enginetypeohcf + enginetyperotor + 
               CarCompanybmw +
               CarCompanymitsubishi + 
               CarCompanytoyota, data = train_carprice)


summary(model_17)


# removing variables having p values greater then .0009 that is (2 *)
model_18<-lm(price ~ aspiration + enginelocation +enginesize + stroke  + enginetypeohcf 
             + enginetyperotor + CarCompanybmw +CarCompanytoyota, data = train_carprice)


summary(model_18)



# removing variables having p values greater then .0009 that is (2 *)
model_19<-lm(price ~ aspiration + enginelocation +enginesize + stroke  + enginetypeohcf 
             + enginetyperotor + CarCompanybmw , data = train_carprice)

summary(model_19)



#similarly removing enginetypeohcf and checking the impact on R square, it decreases from .9021 to .8932
#hence it can be removed.

model_20<-lm(price ~ aspiration + enginelocation +enginesize + stroke  
             + enginetyperotor + CarCompanybmw , data = train_carprice)

summary(model_20)


#The model contains 6 vairables which has high significance and Adjusted R-squared:  0.8806
#Hence this model can be considered.The value of adjusted R sqaure is .8932 which can be considered as very good for the model.

#1.aspiration 
#2.enginelocation
#3.enginesize 
#4.stroke
#5.enginetyperotor
#6.CarCompanybmw


# Predict the car prices in the testing dataset
Predict_1 <- predict(model_20,test_carprice[,-1])
test_carprice$test_carprice <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test_carprice$price,test_carprice$test_carprice)#.8850
# calculate R squared by squaring correlation
rsquared <- cor(test_carprice$price,test_carprice$test_carprice)^2#.7833

#The r sqaured is .7833 hence the model looks good for testing data as well.

#Deriving the below columns to find plot of actual and predicted data along with error plot.
train_carprice$Predicted_carprice <- predict(model_20, train_carprice)
train_carprice$error <-  train_carprice$price - train_carprice$Predicted_carprice 


# Plot - Actual vs Predicted Views Model20
ggplot(train_carprice, aes(Predicted_carprice, price)) + geom_smooth()

#We could find that actual price and predicted price are almost same as straight line
#passes through it.


# Plot Model20 errors
ggplot(train_carprice, aes(enginesize, error)) + geom_point() 
#The error point does not have any pattern and are randomly distributed, hence we can
#conclude that module is accurate including all the required parameters.