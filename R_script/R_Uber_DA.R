library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)


setwd("D:/Users/Sriram/Desktop/PythonR/")

colors = c(""#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"")
         ? 
           apr_data <- read.csv("uber-raw-data-apr14.csv")
           may_data <- read.csv("uber-raw-data-may14.csv")
           jun_data <- read.csv("uber-raw-data-jun14.csv")
           jul_data <- read.csv("uber-raw-data-jul14.csv")
           aug_dat? <- read.csv("uber-raw-data-aug14.csv")
           sep_data <- read.csv("uber-raw-data-sep14.csv")
           
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)     
head(data_2014)

           
data_2014$Date.Time <- as.POSIXct?data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

head(data_2014$Date.Time)

#separating Time from date time
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

head(data_2014)

data_2014$Date.Time ?- ymd_hms(data_2014$Date.Time)

head(data_2014)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(da?a_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#we will use the ggplot function to plot the number of tr?ps that the passengers had made in a day. 
#We will also use dplyr to aggregate our data. 
#In the resulting visualizations, we can understand how the number of passengers fares throughout the day. 
#We observe that the number of trips are higher in the ev?ning around 5:00 and 6:00 PM.

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarise(Total = n())

datatable(hour_data)


#ggplot viz
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ?gtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())


ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_b?r( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)
  
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( s?at = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())



day_month_group ?- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill?manual(values = )

#Number of Trips taking place during months in a year

#In this section, we will visualize the number of trips that are taking place each month of the year. 
#In the output visualization, we observe that most trips were made during the m?nth of September. 
#Furthermore, we also obtain visual reports of the number of trips that were made on every day of the week.

month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot(month_group, ?es(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#CC1011", "#665555", "#05a399","#cfcaca", "#f5e840"? "#0683c9","#e075b0"))


month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by?Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#CC1011", "#665555", "#05a399","#cfcaca", "#f5e840", "#0683c9","#e075b0"))


#Finding out the number of Trips by bases
#In the following visualization, we plot the num?er of trips that have been taken by the passengers from each of the bases. 
#There are five bases in all out of which, we observe that B02617 had the highest number of trips. 
#Furthermore, this base had the highest number of trips in the month B02617. 
#T?ursday observed highest trips in the three bases - B02598, B02617, B02682.

ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkblue") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(data_2014, aes(Base, fill = month)) + 
  ?eom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = c("#CC1011", "#665555", "#05a399","#cfcaca", "#f5e840", "#0683c9","#e075b0"))

ggplot(data_2014, aes(Base, fill = dayo?week)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = c("#CC1011", "#665555", "#05a399","#cfcaca", "#f5e840", "#0683c9","#e075b0"))

#Creating a Heatmap vi?ualization of day, hour and month
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)


ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by H?ur and Day")

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")


ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map b? Month and Day of Week")

#Creating a map visualization of rides in New York
#we will visualize the rides in New York city by creating a geo-plot that will help us to visualize the rides 
#during 2014 (Apr - Sep) and by the bases in the same period.
min_la? <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

#for theme-map()
library(cowplot) 

ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continu?us(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_cont?nuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

