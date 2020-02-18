
#--------------Load the Uber Data Set provided-----------------------

Uber_data <- read.csv("Uber Request Data.csv")





# *************************Data Cleaning and Preparation*********************************

#------------------------------------Coverting Date and time in the proper format------------------------


library(stringr)

Uber_data$Drop.timestamp<- str_replace_all(Uber_data$Drop.timestamp, "/", "-")

Uber_data$Drop.timestamp = as.POSIXct(Uber_data$Drop.timestamp, format = "%d-%m-%Y %H:%M")


Uber_data$Request.timestamp<- str_replace_all(Uber_data$Request.timestamp, "/", "-")

Uber_data$Request.timestamp = as.POSIXct(Uber_data$Request.timestamp, format = "%d-%m-%Y %H:%M")





#*******************************Deriving new variables which will be useful for analysis.**************************************


Uber_data$Request_Date <- format(as.POSIXct(Uber_data$Request.timestamp,format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")

Uber_data$Request_Time <- format(as.POSIXct(Uber_data$Request.timestamp,format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")

Uber_data$Request_hour <- format(Uber_data$Request.timestamp, "%H")

Uber_data$Request_hour <- as.numeric(Uber_data$Request_hour)

#--------Deriving a new column named "Time Slot" on the basis of Request_hour--------

Uber_data$Time_Slot[Uber_data$Request_hour <= 3] <- c("Pre_Morning")

Uber_data$Time_Slot[((Uber_data$Request_hour >= 4) & (Uber_data$Request_hour <= 9))] <- c("Morning_Rush")
 
Uber_data$Time_Slot[((Uber_data$Request_hour >= 10) & (Uber_data$Request_hour <= 16))] <- c("Day_Time")

Uber_data$Time_Slot[((Uber_data$Request_hour >= 17) & (Uber_data$Request_hour <= 21))] <- c("Evening_Rush")
 
Uber_data$Time_Slot[which(is.na(Uber_data$Time_Slot==T))] <- c("Late_Night")
# 

#--------Calculating Trips Completed with in specific time slot like 'Day Time' derived above like---------

Day_Time_count <- length(which((Uber_data$Time_Slot=="Day_Time") & (Uber_data$Status == "Trip Completed")))

Pre_Morning_count <- length(which((Uber_data$Time_Slot=="Pre_Morning") & (Uber_data$Status == "Trip Completed")))

Morning_Rush_count <- length(which((Uber_data$Time_Slot=="Morning_Rush") & (Uber_data$Status == "Trip Completed")))

Evening_Rush_count <- length(which((Uber_data$Time_Slot=="Evening_Rush") & (Uber_data$Status == "Trip Completed")))

Late_Night_count <- length(which((Uber_data$Time_Slot=="Late_Night") & (Uber_data$Status == "Trip Completed")))



require(dplyr)
require(ggplot2)
require(scales)




Uber_data$Request_hour <- as.numeric(Uber_data$Request_hour)


# Bar Chart depicting hour wise trip request made at city and airport respectively.


Hourwise_Request_Count <- ggplot(Uber_data,aes(x=factor(Request_hour),fill=factor(Pickup.point)))

Plot1 <- Hourwise_Request_Count+geom_bar(stat="count",position = "dodge")+ggtitle("Uber Cabs Hourly Demand")+labs(x="Time in Hours", y="Number of Cabs Requested")

Plot1


# Bar Chart for number of trips made during different time slots

trips_completed <- subset(Uber_data,Uber_data$Status=="Trip Completed")

Timeslot_bar <- ggplot(trips_completed,aes(x=Time_Slot))

Plot2 <- Timeslot_bar+geom_bar(stat="count",col="black",fill="pink")+
  ggtitle("Trips completed during different Time Slots")+
  labs(x="Time Slots",y="Trips Completed")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  guides(fill=FALSE)+
  scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))

Plot2


# Dodged bar chart here representing portion of bars as completed, cancelled and no cars available requests out of total requests made

Timeslot_Request_Count <- ggplot(Uber_data,aes(x=factor(Time_Slot),fill=factor(Status)))

Plot3 <- Timeslot_Request_Count+geom_bar(stat="count",position = "dodge",col="black")+
  ggtitle("Trips during Different Time Slots")+
  scale_x_discrete(limits=c("Evening_Rush","Morning_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))+
  labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
  scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))

Plot3



#problem 1. Large number of service requests got cancelled during the Morning_Rush Time slot

#Subset the Morning Rush time slot data for analysis
Problem_df <- subset(Uber_data,Uber_data$Time_Slot=="Morning_Rush")


#Plot the bargraph with status of request in x-axis and count in y-axis for Morning rush time slot
#Show the request from different pickup points in different colors

Problem1_count <- ggplot(Problem_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot4 <- Problem1_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Morning Rush Cab Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
  annotate("text", x=-Inf,y=Inf,label="Airport - 2.79% & City = 97.20%", hjust=-.1,vjust=1)
  
#view the plot
plot4



#*************Morniing_Rush airport and city trip calculation*********************

#Number of trips cancelled for the Morning rush time slot
Total_Trip_Cancelled <- length(which(Problem_df$Status=="Cancelled"))

#Number of trips cancelled from airport for Morning rush
Airport_Trip_cancel <- length(which((Problem_df$Pickup.point=="Airport") & (Problem_df$Status == "Cancelled")))

# Number of trips cancelled from city for Morning rush
City_Trip_Cancel <- length(which((Problem_df$Pickup.point=="City") & (Problem_df$Status == "Cancelled")))

# Percentage of trips cancelled from city out of total trips cancelled during morning rush
Percent_Trip_Cancel_City <- ((City_Trip_Cancel/Total_Trip_Cancelled)*100)

# Percentage of trips cancelled from airport out of total trips cancelled during Morning rush
Percent_Trip_Cancel_Airport <- ((Airport_Trip_cancel/Total_Trip_Cancelled)*100)

# Number of trips requested from city to airport during morning rush
Demand_Trip_Request_City <- length(which(Problem_df$Pickup.point=="City"))

#Number of trips completed from city to airport during morning rush
Demand_Trip_City_Completed <- length(which((Problem_df$Pickup.point=="City")& (Problem_df$Status=="Trip Completed")))






#*************Evening_Rush airport and city no cab availablility calculation*********************



#problem2 - Cabs were not available for a large number of requests during Evening_Rush time slot


#subset the data for Evening rush from dataframe for analysis
Problem2_df <- subset(subset(Uber_data, Uber_data$Time_Slot=="Evening_Rush"))

#plot the bar graph with status of requests on x-axis and count in y-axis for evening rush time slot
# Show the request from different pickup points in different colors

Problem2_count <- ggplot(Problem2_df,aes(x=factor(Status),fill=factor(Pickup.point)))

plot5 <- Problem2_count+geom_bar(stat="count",position = "dodge")+
  ggtitle("Evening Rush Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
  annotate("text", x=-Inf,y=Inf,label="Airport - 95.89% & City = 5.10%", hjust=-.1,vjust=1)  
#view the plot
plot5


# No of service requests with no cars available for evening rush time slot

total_nocar_available <- length(which(Problem2_df$Status=="No Cars Available"))
# No of  service requests with no cars available from airport during evening rush
airport_nocar_available <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status == "No Cars Available")))
# No of service requests with no cars availablefrom city during evening rush
city_nocar_available <- length(which((Problem2_df$Pickup.point=="City") & (Problem2_df$Status == "No Cars Available")))
# Percentage of no cars available status from city out of total no cars available during evening rush
percent_city_nocar <- (city_nocar_available/total_nocar_available*100)
# Percentage of no cars available status from airport out of total no cars available during evening rush
percent_airport_nocar <- (airport_nocar_available/total_nocar_available*100)
#No of service requests from airport to city during evening rush
demand_nocar_request_airport <- length(which(Problem2_df$Pickup.point=="Airport"))
#No of trips completed from airport to city during evening rush
demand_nocar_request_airport_completed <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status=="Trip Completed")))



#**************************************************End of Code*******************************************************#

