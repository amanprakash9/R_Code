#setwd("D:/IIIT-B/ts casestudy")
# setwd("~/DS/timeseries assignment - global mart sales prediction")
library(forecast)
library(tseries)
library(forecast)
library(tseries)
require(graphics)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


##
#To Forecast Sales and Demand For Next 6 Months
##

#### reading the global store dataset in to data frame
Global_Superstore <- read.csv("Global Superstore.csv",stringsAsFactors = F)
str(Global_Superstore)
summary(Global_Superstore)

ncol(Global_Superstore)
#24 attributes

nrow(Global_Superstore)
#51290 observations
#sanity check   
colSums(is.na(Global_Superstore))
#only postal code has 41296 NA
# 
missing_values <- Global_Superstore %>%summarise_all(funs(sum(is.na(.))/n()))
missing_values
#postal code has very high na valuse and should be removed
table(duplicated(Global_Superstore))
#no duplicate 

########
#selecting only required columns which are needed for segmentation and time series model build
#########
#
gs=Global_Superstore[,c(3,8,13,19,20,22)]


########combining market and segment to create new column with 21 unique levels
gs$market_segment=paste(gs$Segment,gs$Market,sep = "_")
gs=gs[,-c(2,3)]



#library(lubridate)
#removing day part from date

gs=separate(gs,col =Order.Date,into = c("date","month","year"),"-" )
gs$mon_year=paste(gs$month,gs$year,sep = "-")
gs=gs[,-c(1,2,3)]


#library(dplyr)
#find the total profit for each market segment
# group by on 21 market segments and than on each month-year
gs0=gs%>%
  group_by(market_segment)%>%
  summarise(profit_sum=sum(Profit),Sales_sum=sum(Sales),Quantity_sum=sum(Quantity))%>%arrange(desc(profit_sum))
gs0
#The below two seems most profitable market segment
#Consumer_APAC
# Consumer_EU 
#
# group by on 21 market segments and than on each month-year
gs1=gs%>%
  group_by(market_segment,mon_year)%>%
  summarise(profit_sum=sum(Profit),Sales_sum=sum(Sales),Quantity_sum=sum(Quantity))

plot

# getting profit coefficient of each segment on monthly aggregated value
gs2=gs1%>%
  group_by(market_segment)%>%
  summarise(profit_coefficient=(sd(profit_sum)/mean(profit_sum)))

#ordering
gs2=gs2[order(gs2$profit_coefficient),]    # 2 segment with least coefficient
#Consumer_eu &  #Consumer_apac
gs2

#from gs2 figures we can see the most profitable and consistent 
#market segements are Consumer_EU and Consumer_APAC
#
#so we will focus on these 2 market segments for time series analysis
#
# converting into date time format and ordering as timeseries data needs to be in sequence
gs1$mon_year=dmy(paste0("01-",gs1$mon_year))

gs1=gs1[order(gs1$mon_year),]


# creating 21 data frame or seperating 21 market , segments 
uni=unique(gs1$market_segment)

for(i in uni){
  assign(paste0("df",i),gs1[gs1$market_segment==i,])
}

########################
#checking Consumer EU data frame
#######################
str(dfConsumer_EU)
summary(dfConsumer_EU)ss

#time series conversion for Sales for Consumer EU

#time series conversion for Sales
ts_consumer_eu_Sales=ts(dfConsumer_EU$Sales_sum)
dfConsumer_EU$MonthNum<-c(1:nrow(dfConsumer_EU))

ts_consumer_eu_Sales<-dfConsumer_EU[,c("MonthNum","Sales_sum")]
consumer_eu_comp.timeser_Sales<-ts(ts_consumer_eu_Sales$Sales_sum)

ts_consumer_eu_Sales_tr <- ts_consumer_eu_Sales[1:(nrow(ts_consumer_eu_Sales) - 6),]

consumer_eu.timeser_Sales<-ts(ts_consumer_eu_Sales_tr$Sales_sum)
plot(consumer_eu.timeser_Sales)

# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
consumer_eu.timeser_Sales.d<-ts(consumer_eu.timeser_Sales,frequency=12)
consumer_eu.timeser_Sales.d.decompose <- decompose(consumer_eu.timeser_Sales.d)
plot(consumer_eu.timeser_Sales.d.decompose)

#Hence the time series has seasonal as well as trend componenet.


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(consumer_eu.timeser_Sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer_eu.timeser_Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- ts_consumer_eu_Sales_tr$MonthNum
lines(smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)



#locally predictable series
#We will model it as an ARMA series by classical decompostion method

local_pred <- consumer_eu.timeser_Sales - global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Hence the series does not have any locally predicted part.


#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The value is 0.02 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

ts_consumer_eu_Sales<- data.frame(ts_consumer_eu_Sales)

#Taking out last 6 months data to compare the actual and predicted values.
outdata <- ts_consumer_eu_Sales[43:48,]
outdata$Month <- outdata$MonthNum
colnames(outdata)[1] <- "Month"
timevals_out <- outdata$Month


global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Comparing prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#The value of MAPE is 67.63

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(consumer_eu_comp.timeser_Sales, col = "black")
lines(class_dec_pred, col = "red")

#The prediction is correct for last 6 month the value is increasing then constant then decreasing.

#ARIMA fit

autoarima <- auto.arima(consumer_eu.timeser_Sales)
autoarima

#(0,1,1) HENCE DIFFERENCING PART AND AUTO REGRESSIVE  PART IS PRESENT BUT MOVING AVERAGE
#PART IS NOT PRESENT


tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer_eu.timeser_Sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#The value is 0.01 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi_auto_arima)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(consumer_eu_comp.timeser_Sales, col = "black")
lines(auto_arima_pred, col = "red")


#The MAPE value is less for ARIMA model as compared to classical model , hence ARIMA
#modelling will be prefreed.





#time series conversion for Quantity for Consumer EU

#time series conversion for Quantity
ts_consumer_eu_Quantity=ts(dfConsumer_EU$Quantity_sum)
dfConsumer_EU$MonthNum<-c(1:nrow(dfConsumer_EU))

ts_consumer_eu_Quantity<-dfConsumer_EU[,c("MonthNum","Quantity_sum")]
consumer_eu_comp.timeser_Quantity<-ts(ts_consumer_eu_Quantity$Quantity_sum)

#Selecting rows for building except last 6 rows.
ts_consumer_eu_Quantity_tr <- ts_consumer_eu_Quantity[1:(nrow(ts_consumer_eu_Quantity) - 6),]

consumer_eu.timeser_Quantity<-ts(ts_consumer_eu_Quantity_tr$Quantity_sum)
plot(consumer_eu.timeser_Quantity)

# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
consumer_eu.timeser_Quantity.d<-ts(consumer_eu.timeser_Quantity,frequency=12)
consumer_eu.timeser_Quantity.d.decompose <- decompose(consumer_eu.timeser_Quantity.d)
plot(consumer_eu.timeser_Quantity.d.decompose)

#Hence the time series has seasonal as well as trend componenet.


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(consumer_eu.timeser_Quantity, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


#Smoothing left end of the time series
ts_consumer_eu_Quantity
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer_eu.timeser_Quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- ts_consumer_eu_Quantity_tr$MonthNum
lines(smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)


#locally predictable series
#We will model it as an ARMA series by classical decompostion method

local_pred <- consumer_eu.timeser_Quantity-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Hence the series does not have any locally predicted part according to clasical
#arima model


#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The value is 0.02 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

ts_consumer_eu_Quantity<- data.frame(ts_consumer_eu_Quantity)

#Taking out last 6 months data to compare the actual and predicted values.
outdata <- ts_consumer_eu_Quantity[43:48,]

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Comparing prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#The value of MAPE is 65.19

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(consumer_eu_comp.timeser_Quantity, col = "black")
lines(class_dec_pred, col = "red")

#The prediction is correct for last 6 month the value is increasing then constant then decreasing.

#ARIMA fit

autoarima <- auto.arima(consumer_eu.timeser_Quantity)
autoarima

#(0,1,1) HENCE DIFFERENCING PART AND AUTO REGRESSIVE  PART IS PRESENT BUT MOVING AVERAGE
#PART IS NOT PRESENT

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer_eu.timeser_Quantity - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#The value is 0.01 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi_auto_arima)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(consumer_eu_comp.timeser_Quantity, col = "black")
lines(auto_arima_pred, col = "red")


#The MAPE value is less for ARIMA model as compared to classical model , hence ARIMA
#modelling will be prefreed.





#time series conversion for Sales for Consumer APAC

#time series conversion for Sales
ts_consumer_APAC_Sales=ts(dfConsumer_APAC$Sales_sum)
dfConsumer_APAC$MonthNum<-c(1:nrow(dfConsumer_APAC))

ts_consumer_APAC_Sales<-dfConsumer_APAC[,c("MonthNum","Sales_sum")]
consumer_APAC_comp.timeser_Sales<-ts(ts_consumer_APAC_Sales$Sales_sum)

ts_consumer_APAC_Sales_tr <- ts_consumer_APAC_Sales[1:(nrow(ts_consumer_APAC_Sales) - 6),]

consumer_APAC.timeser_Sales<-ts(ts_consumer_APAC_Sales_tr$Sales_sum)
plot(consumer_APAC.timeser_Sales)

# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
consumer_APAC.timeser_Sales.d<-ts(consumer_APAC.timeser_Sales,frequency=12)
consumer_APAC.timeser_Sales.d.decompose <- decompose(consumer_APAC.timeser_Sales.d)
plot(consumer_APAC.timeser_Sales.d.decompose)

#Hence the time series has seasonal as well as trend componenet.


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(consumer_APAC.timeser_Sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


#Smoothing left end of the time series
ts_consumer_APAC_Sales
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer_APAC.timeser_Sales)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- ts_consumer_APAC_Sales_tr$MonthNum
lines(smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)


#locally predictable series
#We will model it as an ARMA series by classical decompostion method

local_pred <- consumer_APAC.timeser_Sales-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Hence the series does not have any locally predicted part.


#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The value is 0.02 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

ts_consumer_APAC_Sales<- data.frame(ts_consumer_APAC_Sales)

#Taking out last 6 months data to compare the actual and predicted values.
outdata <- ts_consumer_APAC_Sales[43:48,]
outdata$Month <- outdata$MonthNum
colnames(outdata)[1] <- "Month"
timevals_out <- outdata$Month


global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Comparing prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#The value of MAPE is 67.63

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(consumer_APAC_comp.timeser_Sales, col = "black")
lines(class_dec_pred, col = "red")

#The prediction is correct for last 6 month the value is increasing then constant then decreasing.

#ARIMA fit

autoarima <- auto.arima(consumer_APAC.timeser_Sales)
autoarima
#(0,1,1) HENCE DIFFERENCING PART AND MOVING AVERAGE PART IS PRESENT BUT AUTO REGRESSIVE
#PART IS NOT PRESENT

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer_APAC.timeser_Sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#The value is 0.01 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi_auto_arima)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(consumer_APAC_comp.timeser_Sales, col = "black")
lines(auto_arima_pred, col = "red")


#The MAPE value is less for ARIMA model as compared to classical model , hence ARIMA
#modelling will be prefreed.







#time series conversion for Quantity for Consumer apac

#time series conversion for Quantity
ts_consumer_APAC_Quantity=ts(dfConsumer_APAC$Quantity_sum)
dfConsumer_APAC$MonthNum<-c(1:nrow(dfConsumer_APAC))

ts_consumer_APAC_Quantity<-dfConsumer_APAC[,c("MonthNum","Quantity_sum")]
consumer_APAC_comp.timeser_Quantity<-ts(ts_consumer_APAC_Quantity$Quantity_sum)

#Selecting rows for building except last 6 rows.
ts_consumer_APAC_Quantity_tr <- ts_consumer_APAC_Quantity[1:(nrow(ts_consumer_APAC_Quantity) - 6),]

consumer_APAC.timeser_Quantity<-ts(ts_consumer_APAC_Quantity_tr$Quantity_sum)
plot(consumer_APAC.timeser_Quantity)

# Decompose timeseries to see the components
# Added frequency = 12 because decomposition can't happen with frequency = 1
consumer_APAC.timeser_Quantity.d<-ts(consumer_APAC.timeser_Quantity,frequency=12)
consumer_APAC.timeser_Quantity.d.decompose <- decompose(consumer_APAC.timeser_Quantity.d)
plot(consumer_APAC.timeser_Quantity.d.decompose)

#Hence the time series has seasonal as well as trend componenet.


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(consumer_APAC.timeser_Quantity, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


#Smoothing left end of the time series
ts_consumer_APAC_Quantity
diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer_APAC.timeser_Quantity)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- ts_consumer_APAC_Quantity_tr$MonthNum
lines(smoothedseries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)


#locally predictable series
#We will model it as an ARMA series by classical decompostion method

local_pred <- consumer_APAC.timeser_Quantity-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Hence the series does not have any locally predicted part according to clasical
#arima model


#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The value is 0.02 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

ts_consumer_APAC_Quantity<- data.frame(ts_consumer_APAC_Quantity)

#Taking out last 6 months data to compare the actual and predicted values.
outdata <- ts_consumer_APAC_Quantity[43:48,]

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Comparing prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#The value of MAPE is 57.79

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(consumer_APAC_comp.timeser_Quantity, col = "black")
lines(class_dec_pred, col = "red")

#The prediction is correct for last 6 month the value is increasing then constant then decreasing.

#ARIMA fit

autoarima <- auto.arima(consumer_APAC.timeser_Quantity)
autoarima
#(0,1,0) HENCE NO AUTO REGRESSIVE OR MOVING AVERAGE PART BUT DIFFERENCING PART IS PRESENT

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer_APAC.timeser_Quantity - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#The value is 0.01 less then 0.05 hence null hypothesis is rejected and the series 
#is stationary

kpss.test(resi_auto_arima)
#The value is 0.1 more then 0.05 hence null hypothesis is accepted and the series 
#is stationary


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
#26.24


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(consumer_APAC_comp.timeser_Quantity, col = "black")
lines(auto_arima_pred, col = "red")


#The MAPE value is less for ARIMA model as compared to classical model , hence ARIMA
#modelling will be prefreed.


