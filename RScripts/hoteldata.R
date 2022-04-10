setwd("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA")

library(pacman)
library(igraph)

pacman::p_load(data.table, ggplot2, rstudioapi, DataExplorer, bit64, boot)

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
(boxplot_accomtype <- ggplot(data= hotel_data, aes(x= hotel_data$accommodation_type, y=hotel_data$price))+
  geom_boxplot()+geom_jitter())

#kernel density plot
ggplot(data=hotel_data, aes(x=price))+geom_density()
#to explain the bumps in kernel plot
max(hotel_data$price)
sort(hotel_data$price, decreasing = T)[1:10] 

#A violin plot is a compact display of a continuous distribution. 
#It is a blend of geom_boxplot() and geom_density(): a violin plot is a mirrored density plot displayed in the same way as a boxplot.
ggplot(data= hotel_data, aes(x= hotel_data$accommodation_type, y=hotel_data$price))+
  geom_violin() + geom_jitter()
#kernel also gets better with more information

#subsetting data for only hotels in Vienna
(vienna_hotels <- hotel_data[hotel_data$accommodation_type=="Hotel",])

corr(vienna_hotels[,c("price", "distance")]) #negative correlation between price and distance
#regress price on distance
reg1 <- lm(data= vienna_hotels, price ~distance) #give non-robust standard erros
summary(reg1) 

library(sandwich) #for robust estimators

vcov_price_distance <- vcovHC(reg1, type="HC0") #heterosc robust estimator vcovHC or vcovHAC produce different se
vcov_price_distance
sqrt(diag(vcov_price_distance)) #extract standard errorss

#use HC standard errors
library(lmtest)
coeftest(reg1, vcov.=vcov_price_distance) #z and t Wald test, check if robust estimators are still significant

library(boot) 
coefboot <- function(data, idx){ #index idx
  coef(lm(price~distance, data=data[idx,])) #idx specifies set of observations
}

B <- boot(vienna_hotels, coefboot, R=1000) # we took 1000 bootstrap samples and calculated intercept+slope
head(B)

bootdata <- B$t[,2]
bootdata <- as.data.frame(bootdata)

ggplot(data= bootdata, aes(x=bootdata))+ #bootstrap distribution of slope coefficient
  geom_histogram(binwidth = 0.5)+ theme_bw()

#chapter 2
plot(vienna_hotels$distance, vienna_hotels$price)
abline(a=coef(reg1)[1], b=coef(reg1)[2], lwd=3, col="red")

logprice <- log(vienna_hotels$price)
logdist <- log(vienna_hotels$distance)

#log level
reg2 <- lm(data=vienna_hotels, logprice~vienna_hotels$distance); summary(reg2)
plot(vienna_hotels$distance, logprice)
abline(a=coef(reg2)[1], b=coef(reg2)[2], lwd=3, col="red")

#level log
plot(logdist, vienna_hotels$price)
abline(a=coef(reg3)[1], b=coef(reg3)[2], lwd=3, col="chocolate")
# reg3 <- lm(vienna_hotels$price~logdist); summary(reg3) #not a good idea,
which(vienna_hotels$distance==0) #because obs 75 is 0 distance

reg3 <- lm(vienna_hotels$price[-75]~logdist[-75]); summary(reg3) #eliminate -75 from set for regression

#log log has best specification looking at r squared
reg4 <- lm(logprice[-75]~logdist[-75]); summary(reg4)
plot(logdist[-75],logprice[-75])
abline(a=coef(reg4)[1], b=coef(reg4)[2], lwd=3, col="cadetblue")

#adj reg1
reg1_adj <- lm(data= vienna_hotels, price[-75] ~distance[-75]); summary(reg1_adj)
plot(vienna_hotels$distance[-75], vienna_hotels$price[-75])
abline(a=coef(reg1_adj)[1], b=coef(reg1_adj)[2], lwd=3, col="steelblue")
