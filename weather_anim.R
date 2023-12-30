#load packages----
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(gganimate)
library(gifski)

#setting working directory----
setwd("G:/quarantine/data/")

# Importing data 
weather= read.csv("Weather.csv", stringsAsFactors = F)
View(data)
names(data)
names(weather)

# Necessary wranglings----
data= weather %>% mutate(date= date(date), year= year(date), month= month(date, label = T)) %>% 
  filter(year== 2016 & month== "Mar" & province== "Seoul") %>%
  gather(var, value, c(avg_temp, min_temp, max_temp)) %>%
  select(-province, -code, -precipitation, -max_wind_speed)

View(data)

class(data$date)

# Multiple line graph for Seoul
data %>% ggplot(aes(x= date(date), y= value, colour= var))+
  geom_line()

#Necessary wranglings----
data= weather %>% mutate(date= date(date), year= year(date), month= month(date, label = T)) %>% 
  filter(year== 2016 & month== "Mar") %>%
  gather(var, value, c(avg_temp, min_temp, max_temp)) %>%
  select(-avg_relative_humidity, -most_wind_direction, -code, -precipitation, -max_wind_speed)

View(data)


data %>% ggplot(aes(x= date, y= value, colour= var))+
  geom_line(size= 1)+
  facet_wrap(~province)+
  theme(axis.text.x = element_text(angle = 90))


# Animation line graph----
p= data %>% ggplot(aes(x= date, y= value, colour= var))+
  geom_line(size= 1)+
  facet_wrap(~province)+
  transition_reveal(date)+
  labs(title = "Temperature in Different States of South Korea in March-2016",
       x= "Date", y= "Degree Centigrad")+
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(angle = 90))
p

animate(p, nframes= 50, render= gifski_renderer("South korea tem.gif"),
        height= 500, width= 800)

# Animation bar graph----
p= data %>% ggplot(aes(x= var, y= value, fill= var))+
  geom_col()+
  facet_wrap(~province)+
  transition_reveal(date)+
  labs(title = "Temperature in Different States of South Korea in March-2016",
       x= "Date", y= "Degree Centigrad")+
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(angle = 90))
p

animate(p, nframes= 100, render= gifski_renderer("South korea tem bar.gif"),
        height= 500, width= 800)
