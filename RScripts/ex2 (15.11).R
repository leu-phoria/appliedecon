setwd("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA")

library(pacman)
library(igraph)

pacman::p_load(data.table, ggplot2, rstudioapi, DataExplorer, bit64, boot)
data_repo <- paste0(getwd(),"/data")

#read data
# hotels_europe_price <- read.csv("hotels-europe_price.csv")
# hotels_europe_features <- read.csv("hotels-europe_features.csv")
hotels_europe_price <- fread(paste0("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA/hotels-europe_price.csv"))
hotels_europe_features <- fread(paste0("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA/hotels-europe_features.csv"))

#merge tables left join, we keep the price and merge through ID
hotels_data_all <- merge(hotels_europe_price, hotels_europe_features, by="hotel_id")
hotels_data_all
summary(hotels_data_all)

#subset data, specific rows + all columns, runde Klammern außen um eine Vorschau zu bekommen
(hotel_data <- hotels_data_all[(year=="2017" & month=="11" & weekend =="0" & city=="Vienna"),])

library(DataExplorer)
create_report(hotel_data) #receive a pretty data profiling report, it's helpful to identify missing data

#R goes into the function, executes function and then shows hist_price
change_binwidth_histplot <- function(binwidth){
  hist_price <- ggplot(data= hotel_data, aes(x=price))+ 
    geom_histogram(binwidth = binwidth, size=0.5, alpha=0.8, show.legend = T, na.rm=T)+
    labs(x="Price (US Dollars)", y= "Frequency")
  return(hist_price)
}

#can't see how much data is in there in simple boxplot, so add geom_jitter
boxplot_accomtype <- ggplot(data= hotel_data, aes(x= hotel_data$accommodation_type, y=hotel_data$price))+
  geom_boxplot()+geom_jitter()

#kernel density plot
ggplot(data=hotel_data, aes(x=price))+geom_density()
#to explain the bumps in kernel plot
max(hotel_data$price)
sort(hotel_data$price, decreasing = T)[1:10] 

