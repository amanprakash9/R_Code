
# installing the required packages.
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
library("lubridate")
library("dplyr")
library("ggplot2")

#Exporting the data from csv file.
uber <- read.csv("Uber Request Data.csv")

#Date Cleaning & correcting date format
uber$Request.timestamp<-  parse_date_time(uber$Request.timestamp,orders = c("%d/%m/%Y %H:%M","%d/%m/%Y %H:%M:%S"),tz = "Asia/Calcutta")
uber$Drop.timestamp<-  parse_date_time(uber$Drop.timestamp,orders = c("%d/%m/%Y %H:%M","%d/%m/%Y %H:%M:%S"),tz = "Asia/Calcutta")

#Creating a new column hour for both request and drop
uber$Request.timestamp.hour <- format(uber$Request.timestamp, "%H")
uber$Drop.timestamp.hour <- format(uber$Drop.timestamp, "%H")

#Converting the column for further use.
uber$Request.timestamp.hour  = as.numeric(uber$Request.timestamp.hour)


#Creating a timeslot column based on below criteria



 # Early morning    4am to 8 am
#Late morning     8 am to 12pm

 # Early afternoon   12pm to 3 pm
 # Late afternoon    3 to 6 pm


 # Early evening   6 to 9 pm
# Late evening    9 pm to 12 pm

 # Night          12 pm to 4 am 
 #Late Night      3 am to 4 am 
  
  
uber$timeslot_req = cut(uber$Request.timestamp.hour, breaks=c(4,8,12,15,18,21,23,0,3)
                ,labels=c("Night" ,"Late Night" ,"Early Morning","Late Morning" , "Early Afternoon" , "Late Afternoon", "Early Evening" ,"Late Evening")
                , include.lowest=TRUE, 
                    right=FALSE)

#Plotting a graph to analyze to find status of request based on timeslot on
#complete data.

ggplot(uber, aes(x= timeslot_req,fill = Status)) + geom_bar()

#grouping the complete data based on timeslot to analyze demand, supply gap and filtering
#on the basis of trips status

timeslot_status_grp <- group_by(uber, timeslot_req , Status)
status_summ <- summarise( timeslot_status_grp, freq=n())


#Subsetting the data based on airport,city pickup's.

uber_arpt_pick <- filter (uber , Pickup.point == "Airport")
uber_city_pick <- filter (uber , Pickup.point == "City")

#Plotting the a graph to analyze to find status of request based on timeslot on
#the basis pickup Request.

ggplot(uber_arpt_pick, aes(x= timeslot_req,fill = Status)) + geom_bar()
ggplot(uber_city_pick, aes(x= timeslot_req,fill = Status)) + geom_bar() 



#grouping the complete data based on timeslot to analyze demand, supply gap and filtering
#on the basis of trips status for airport pickup

timeslot_status_grp_arpt_pick <- group_by(uber_arpt_pick, timeslot_req , Status)
status_summ_arpt_pick <- summarise( timeslot_status_grp_arpt_pick, freq=n())

#grouping the complete data based on timeslot to analyze demand, supply gap and filtering
#on the basis of trips status for city pickup

timeslot_status_grp_city_pick <- group_by(uber_city_pick, timeslot_req , Status)
status_summ_city_pick <- summarise( timeslot_status_grp_city_pick, freq=n())

   
#2.Demand Supply Gap

#Demand Supply ratio for complete data

#The data set gives the total number of trips completed.
uber_comp <- filter (uber , Status == "Trip Completed")


#The ration of trips completed to total number of request.
nrow(uber_comp)/nrow(uber)
#It is .419 which means 41.9% trips are gettitng completed.

#grouping the complete data based on timeslot to analyze demand, supply gap.

timeslot_grp <- group_by(uber, timeslot_req)
total_sum <- summarise( timeslot_grp, freq=n())



#grouping the complete data based on timeslot to analyze demand, supply gap and filtering
#on the basis of trips completed.

timeslot_cmp_grp <- group_by(uber, timeslot_req , Status)
comp_summ <- summarise( timeslot_cmp_grp, freq=n())
comp_summ <- filter (comp_summ , Status == "Trip Completed" )


#obtaining the ratio of trips completed to total request to find demand supply gap.
total_sum$freq_trpcom <- comp_summ$freq
total_sum$sup_dem_ratio <- total_sum$freq_trpcom/total_sum$freq


#Plotting demand supply ratio for different timeslots.
ggplot(total_sum, aes(x= timeslot_req  ,y = sup_dem_ratio)) + geom_point() + geom_text(data=total_sum, aes(x= timeslot_req  ,y = sup_dem_ratio , label = sup_dem_ratio ), size=3)



#grouping the complete data based on timeslot to analyze demand, supply gap 
#for airport pickup.

timeslot_grp_arpt_pick <- group_by(uber_arpt_pick, timeslot_req)
total_sum_arpt_pick <- summarise( timeslot_grp_arpt_pick, freq=n())



#grouping the Trip completed data based on timeslot to analyze demand, supply gap.
timeslot_cmp_grp_arpt_pick <- group_by(uber_arpt_pick, timeslot_req , Status)
comp_summ_arpt_pick <- summarise( timeslot_cmp_grp_arpt_pick, freq=n())
comp_summ_arpt_pick <- filter (comp_summ_arpt_pick , Status == "Trip Completed")


#obtaining the ratio of trips completed to total req to find demand supply gap.
total_sum_arpt_pick$freq_trpcom <- comp_summ_arpt_pick$freq
total_sum_arpt_pick$sup_dem_ratio <- total_sum_arpt_pick$freq_trpcom/total_sum_arpt_pick$freq


#Plotting demand supply ratio for different timeslots for airport pickup.
ggplot(total_sum_arpt_pick, aes(x= timeslot_req  ,y = sup_dem_ratio)) + geom_point() + geom_text(data=total_sum_arpt_pick, aes(x= timeslot_req  ,y = sup_dem_ratio , label = sup_dem_ratio ), size=3) 



#grouping the complete data based on timeslot to analyze demand, supply gap 
#for city pickup.

timeslot_grp_city_pick <- group_by(uber_city_pick, timeslot_req)
total_sum_city_pick <- summarise( timeslot_grp_city_pick, freq=n())



#grouping the Trip completed data based on timeslot to analyze demand, supply gap.
timeslot_cmp_grp_city_pick <- group_by(uber_city_pick, timeslot_req , Status)
comp_summ_city_pick <- summarise( timeslot_cmp_grp_city_pick, freq=n())
comp_summ_city_pick <- filter (comp_summ_city_pick , Status == "Trip Completed")


#obtaining the ratio of trips completed to total req to find demand supply gap.
total_sum_city_pick$freq_trpcom <- comp_summ_city_pick$freq
total_sum_city_pick$sup_dem_ratio <- total_sum_city_pick$freq_trpcom/total_sum_city_pick$freq


#Plotting demand supply ratio for different timeslots for airport pickup.
ggplot(total_sum_city_pick, aes(x= timeslot_req  ,y = sup_dem_ratio)) + geom_point() + geom_text(data=total_sum_city_pick, aes(x= timeslot_req  ,y = sup_dem_ratio , label = sup_dem_ratio ), size=3) 

