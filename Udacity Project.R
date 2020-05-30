#To load plot,read,date packages

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#To remove scientific notations-scipen

options(scipen = 9999)
options(warn = -1)

#To import the csv docs

chi <- read_csv("chicago.csv")
ny <- read_csv("new_york_city.csv")
wash <- read_csv("washington.csv")

#To display the summary of each dataset
sapply(list("Chicago" = chi,"New York" = ny,"Washington"  =  wash), glimpse)

sapply(list("Chicago" = chi,"New York" = ny,"Washington"  =  wash), summary)

chi %>% summarise_all(funs(sum(is.na(.))))
ny %>% summarise_all(funs(sum(is.na(.))))
wash %>% summarise_all(funs(sum(is.na(.))))

ny$`Start Time` <- ymd_hms(ny$`Start Time`)
ny$`End Time` <- ymd_hms(ny$`End Time`)


wash$`Start Time` <- ymd_hms(wash$`Start Time`)
wash$`End Time` <- ymd_hms(wash$`End Time`)


chi$`Start Time` <- ymd_hms(chi$`Start Time`)
chi$`End Time` <- ymd_hms(chi$`End Time`)

#To assign a function extract

extract <- function(data){
  data$hour <- hour(data$`Start Time`)
  data$month <- month(data$`Start Time`)
  data$day <- weekdays(data$`Start Time`)
  return(data)
}

#Creating seperate columns for hour date and month for chicago, washington and new york

chicago <- extract(chi)
newyork <- extract(ny)
washington <- extract(wash)

#Impart orange as fill for the graphs
or = "#E69F00"

#Popular months in chicago, newyork and washington

ggplot(chicago, aes(month)) + geom_histogram(bins = 6, color = I('black'), fill=(or))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,30000, 5000))+
  xlab("month") +  ylab("Count")+ ggtitle("Popular Months in Chicago")

ggplot(newyork, aes(month)) + geom_histogram(bins = 6, color = I('black'), fill=(or))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,30000, 5000))+
  xlab("month") +  ylab("Count")+ ggtitle("Popular Months in New York City")

ggplot(washington, aes(month)) + geom_histogram(bins = 6, color = I('black'), fill=(or))+  
  scale_x_continuous(breaks = seq(1,6,1))+ scale_y_continuous(breaks = seq(0,30000, 5000))+
  xlab("month") +  ylab("Count")+ ggtitle("Popular Months in Washington")
 table(chicago$month)
 table(newyork$month)
 table(washington$month)
#Popular weekdays in chicago, newyork and washington

ggplot(chicago, aes(day)) + geom_bar(color = I('black'), fill=(or))+  
  xlab("Weekdays") +  ylab("Count")+ ggtitle("Popular Weekdays in Chicago")

ggplot(newyork, aes(day)) + geom_bar(color = I('black'), fill=(or))+  
  xlab("Weekdays") +  ylab("Count")+ ggtitle("Popular Weekdays in New York City")

ggplot(washington, aes(day)) + geom_bar(color = I('black'), fill=(or))+  
  xlab("Weekdays") +  ylab("Count")+ ggtitle("Popular Weekdays in Washington")

table(chicago$day)
table(newyork$day)
table(washington$day)

#Popular hours in chicago, newyork and washington

ggplot(chicago, aes(hour)) + geom_histogram(bins = 24, color = I('black'), fill=(or))+  
  scale_x_continuous(breaks = seq(0,23,1))+
  xlab("hour") +  ylab("count")+ ggtitle("Popular hours in Chicago")

ggplot(newyork, aes(hour)) + geom_histogram(bins = 24, color = I('black'), fill=(or))+  
  scale_x_continuous(breaks = seq(0,23,1))+
  xlab("hour") +  ylab("count")+ ggtitle("Popular hours in New York City")

ggplot(washington, aes(hour)) + geom_histogram(bins = 24, color = I('black'), fill=(or))+  
  scale_x_continuous(breaks = seq(0,23,1))+
  xlab("hour") +  ylab("count")+ ggtitle("Popular hours in Washington")

table(chicago$hour)
table(newyork$hour)
table(washington$hour)

#Gender count in chicago and new york

chicago %>% group_by(Gender) %>% filter(!is.na(Gender))%>%
  summarise(total = length(Gender))%>%
  ggplot(aes(Gender, total)) + geom_bar(stat = 'identity', color = I('black'), fill=(or))+
  xlab("Gender") +  ylab("Count ")+ ggtitle("Counts of each gender in chicago")

table(chicago$Gender)


newyork %>% group_by(Gender) %>% filter(!is.na(Gender))%>%
  summarise(total = length(Gender))%>%
  ggplot(aes(Gender, total)) + geom_bar(stat = 'identity', color = I('black'), fill=(or))+
  xlab("Gender") +  ylab("Count ")+ ggtitle("Counts of each gender in new york")
table(newyork$Gender)
