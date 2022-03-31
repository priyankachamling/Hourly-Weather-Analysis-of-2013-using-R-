
# Priyanka Rai
#NP000412

#Checking and setting working directory
getwd()
setwd("C:/Users/user/OneDrive - Lord Buddha Education Foundation/Desktop")
#Data import
Weatherdata <- read.csv("Weatherdata.csv")
View(Weatherdata)

#Pre-processing 
#Step 1
dim(Weatherdata)#For determining no. of rows and columns

#Step 2
names(Weatherdata)#For determining headers of column

#Step 3
str(Weatherdata)#For determining the compact structure

#Step 4
View(Weatherdata)#For visualizing all the data set of csv file

#Step 5
summary(Weatherdata)#For determining each column summary


#Packages
install.packages("IRdisplay") 
library(IRdisplay)

install.packages("corrplot")
library(corrplot)

install.packages("GGally")
library(GGally)

install.packages("plotrix")
library(plotrix)

install.packages("plotly")
library(plotly)

install.packages("boxplotdbl")
library(boxplotdbl) 

install.packages("RColorBrewer")
library(RColorBrewer) 

install.packages("xlsx")
library(xlsx) 

install.packages("ggplot2")
library(ggplot2) #To plot a graph/ data visualization

install.packages("grid")
library(grid) #For graphical object representation

install.packages("dplyr")
library(dplyr) #For sorting data/ data manipulation

install.packages("corrgram")
library(corrgram) #For visualizing correlation matrix

install.packages("viridis")
library(viridis) #For default color maps

install.packages("tidyr")
library(tidyr) #To clean up sloppy data

install.packages("tidyverse")
library(tidyverse) #For data transformation & presentation 

install.packages("hrbrthemes")
library(hrbrthemes) #For additional themes

install.packages("gplots")
library(gplots) #For plotting data

install.packages("reshape2")
library(reshape2) #For transforming data into the desired structure

install.packages("ggridges")
library(ggridges) #For ridge line plots in ggplot2

install.packages("ggExtra")
library(ggExtra) #for ggplot2 enhancement

install.packages("viridis")
library(viridis) #For default color maps

install.packages("ggridges") #For ridge line plots
library(ggridges)


#ANALYSIS 
#Analysis Example 1-Line Plot
#IRdisplay
#ggplot2
#dplyr
#To determine the relationship between pressure and wind speed where x= hour and y=wind speed.
A1 = Weatherdata %>% filter(month == 4 & day == 4) %>%
  ggplot(Weatherdata, mapping = aes(x=hour, y=wind_speed)) + geom_line(aes(group = origin, color = origin))
A1 = A1 + #To modify axis labels
  xlab("Hours on the 4th of April") +
  ylab("Wind speed on the 4th of April")
A1 = A1 + facet_wrap(~origin)
A1 = A1 + labs(title = "Atmospheric pressure per hour")
print(A1)


#Analysis Example 2-Histogram
#ggplot2
#To determine the humidity histogram per day where x value is humid
histo_humid <- ggplot(Weatherdata, mapping = aes(x=humid)) + geom_histogram(col="black", fill="chartreuse")+
  labs(title = 'Humid Histogram of per Day', x = 'humid') +
  facet_wrap(~day)
print(histo_humid)


#Analysis Example 3-Bubble Plot
#ggplot2
#For monitoring the dew point to determine density where x = month and y = day
A3 = Weatherdata %>% 
  ggplot(aes(x=month, y=day, size=dewp, color = origin)) + geom_point(alpha = 0.5)+
  ylab("Days in month")+
  xlab("Months")
A3 = A3 + ggtitle("Dew point during each day of the year")
A3 = A3 + facet_wrap(~origin)
print(A3)

#Analysis Example 4-Lollipop Chart
#ggplot2
#dplyr
#Mist formation by analyzing the temperature against humidity where x=month and y=day.
A4 = Weatherdata %>% filter(month == 8 & day == 14) %>%
  ggplot(Weatherdata, mapping = aes(x=humid, y=temp))+
  geom_segment(aes(x=humid, xend=humid, y=1, yend=temp), color="black") +
  geom_point(color = "purple", size=4)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  ) +
  xlab("Humidity for 14th of August") +
  ylab("Temperature for 14th August") + ggtitle("Mist Formation")
A4 = A4 + facet_wrap(~origin)
print(A4)
  

#Analysis Example 5-Correlogram
#ggplot2
#Corr plot
#To determine several variables vary with each other(temp, dewp humid, precip, visib).
A5 = data.frame(temp = as.numeric(Weatherdata$temp),dewp = as.numeric(Weatherdata$dewp), 
                humid = as.numeric(Weatherdata$humid), precip = as.numeric(Weatherdata$precip),
                visib = as.numeric(Weatherdata$visib))
corrplot(cor(A5),method = "circle")
print(A5)   

#Analysis Example 6-Heat map
#ggplot2
#Dplyr
#For monitoring the humidity in air per hour where x=day and y=time in hour.
A6 = ggplot(Weatherdata, aes(day, hour)) +
  geom_tile(aes(fill=humid)) +
  scale_fill_gradient2(midpoint = 56, low = "green", mid="blue", high = "red") +
  facet_grid(origin~month)+
  labs(title = ("Humidity in 2013 - JFK & LGA"), x="Day", y="time (Hour)")+
  theme_minimal(base_size = 8)+
  theme(plot.title = element_text(size = 14))+
  theme(axis.text.y = element_text(size = 6))+
  theme(strip.background = element_rect(colour = "black"))+
  theme(plot.title = element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))
print(A6)




#Analysis Example 7-Bar plot
#ggplot2
#dplyr
#To determine the wind direction which may cause delay in flights
A7 = Weatherdata %>% filter(origin == "LGA") %>% filter(month == 12) %>%
  ggplot(Weatherdata, mapping = aes(y=wind_dir))+geom_bar(fill="purple", size=1)
A7 = A7 + #Modify axis labels
  xlab("No. of Times") +
  ylab("Wind direction of LGA on December")
A7 = A7 + labs(title = "Analyzation of wind direction of LGA airport for predicting coming situations")
print(A7)
  
#Analysis Example 8-Density Plot
#RColorBrewer
#ggplot2
#Pressure Density Plot analysis  where pressure is a x value. 
ggplot(data = Weatherdata, mapping = aes(x = pressure, fill = as.factor(Weatherdata$month))) +
  geom_density(alpha = 0.6, colour = "green") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Pressure Density plot") +
  labs(x = "pressure", y = "Density") +
  facet_wrap(~month, scale = "free") +
  theme_grey()

#Analysis Example 9-Box Plot
#ggplot2
#IRdisplay
#dplyr
#To monitor the temperature per month where xlab=months and ylab = Temperature.
Weatherdata = Weatherdata %>%
  mutate(season =
           ifelse(month %in% c(10, 11,12), "Fall",
                  ifelse(month %in% c(4, 5,6), "Spring",
                         ifelse(month %in% c(7, 8,9), "Summer",
                                ifelse(month %in% c(1,2,3), "Winter", "Error")))))
A9 = Weatherdata %>% ggplot(Weatherdata, mapping = aes(month,temp)) + 
  geom_boxplot(aes(group = origin, color = origin),size = 1)
A9 = A9 + scale_x_continuous(breaks = seq(0,12,1))
A9 +
  xlab("Months") +
  ylab("Temperature in airports for the year")
A9 =A9 +facet_wrap(~season)
A9 = A9 + labs(title = "Temperature per seasons")
print(A9)



#Analysis Example 10-Bar Plot
#ggplot2
#To determine the Wind Speed where x value is speed.
origin <- facet_wrap(~origin)
#Visualization & Exploration
h_origin <- ggplot(Weatherdata, mapping = aes(wind_speed, na.rm = TRUE))+
  geom_bar(col="blue",fill="green")+labs(title = 'Wind Speed Bar on Origin', x='Speed')+origin
print(h_origin)


#Analysis Example 11-Density plot
#ggplot2
#ggridges
#To determine the density of Temperature of a year 2013 where x value is temperature and y value is year.
ggplot(Weatherdata, mapping = aes(x = temp, y = year, fill= origin)) +
  geom_density_ridges()+ggtitle("Density  of Temperature of Year")+
  theme_ridges()+
  theme(legend.position = "top")


#Analysis Example 12- Ridgeline Plot
#dplyr
#ggridges
#ggplot2
#For determining the relation between pressure & density where x = pressure and y =wind speed
A12= Weatherdata %>% filter(month == 9 & day == 24) %>%
  ggplot(Weatherdata, mapping = aes(x=pressure, y=as.factor(wind_speed), fill=as.factor(wind_speed)))+
  geom_density_ridges()
  A12 = A12 + #Modify axis labels
  ylab("Wind Speed")
A12 = A12 +labs(title="Relationship Betwwen Pressure and Wind SPeed")
print(A12)


#Analysis Example 13-Scatter Plot
#ggplot2
#tidyr
#Analysis between dewp & humid with reference to origin where x = dewp and y =humid.
li <- stat_smooth(method = 'lm')
splot_h<-ggplot(Weatherdata,
                mapping = aes(x = dewp, y = humid, color = origin)) +
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "pink",
                                    size = 0.5, linetype = "longdash"),
    panel.grid.major = element_line(size = 0.5, linetype = 'longdash',
                                    colour = "gray"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'longdash',
                                    colour = "gray")
  ) +
  geom_point(alpha = 0.4) + li +
  labs(title = 'dewp over humid scatter plot',
       x = 'Dew point', y = 'Humid')
print(splot_h)


#Analysis Example 14-Lollipop Plot
#ggplot2
#To determine Visibility & Precipitation where x value is percip and y = visib.
ggplot(Weatherdata, aes(x=precip, y=visib)) +
  geom_segment(aes(x=precip, xend=precip, y=0, yend=visib), color="blue") + 
  geom_point(color="red", size=4) +
  xlim(0.0,0.2)+
  ylim(0.0,10.0)+
  theme_light()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggtitle("Lolipop Plot")+
  xlab("Precipitation")+
  ylab("Visibility")+
  theme()



#EXTRA FEATURES
#Extra Feature 1-Violin Plot
#ggplot2
#dplyr
#IRdisplay
#To determine the wind direction changing with respect to season where x=season and y=wind direction.
#Manipulation
Weatherdata = Weatherdata %>%
  mutate(season = 
           ifelse(month %in% c(10, 11,12), "Fall",
                  ifelse(month %in% c(4, 5,6), "Spring",
                         ifelse(month %in% c(7, 8,9), "Summer",
                                ifelse(month %in% c(1, 2,3), "Winter","Error")))))
E1 = Weatherdata %>% ggplot(data, mapping = aes(x=season, y=wind_dir, fill=origin)) + 
  geom_violin()
print(E1)


#Extra Feature 2-Plotrix
#ggplot2
#IRdisplay
#dplyr
#Analysis of pressure and humidity relationship where x-lab = Months & y-lab = Average Humidity.
E2 = Weatherdata%>%
  group_by(origin, month)%>%
  summarise(
    mean_pressure = mean(pressure, na.rm = TRUE),
    mean_humid = mean(humid, na.rm= TRUE)
  )
plot(E2$month, E2$mean_humid, col = "red", type = "l", 
     xlab = "Months", ylab = "Average Humidity")
par(new = TRUE)
plot(E2$month, E2$mean_pressure, yaxt= "n", xaxt = "n", 
     ylab = "", xlab = "", col = "Blue", type = "l", main = "Pressure and Humidity in JFK")
axis(side = 4)
mtext("Average Pressure", side = 4)
print(E2)


#Extra Feature 3-Scatter Plot
#ggplot2
#IRdisplay
#dplyr
#Snow usually forms below 32F
#For giving warning in case of takeoff and landing where x=day and y=temperature.
E3 = Weatherdata %>% filter(origin == "JFK") %>% ggplot(Weatherdata, mapping = aes(day,temp))+geom_point(aes(group = month, color = month),size=1)
E3 = E3 + #For modifying axis labels 
  xlab("Days per month") +
  ylab("Temperature in airports during the year")
E3 = E3 + scale_x_continuous(breaks = seq(0,32,4)) + scale_y_continuous(breaks = seq(0,100,20))
E3 = E3 + facet_wrap(~month)
E3 = E3 + annotate("rect", xmin = 0, xmax = 32, ymin = 0, ymax = 32,
                   alpha = .2)
E3 = E3 + labs(title = "Determining the probability of Snowing")
print(E3)

































     










  
