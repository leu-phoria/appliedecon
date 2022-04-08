library(prophet)
Sys.setlocale("LC_TIME","C") #dates in English

#Wikipedia Pageviews for Cuttlefish from 2017-04-06 until 2022-04-07 
df <- read.csv("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA/cuttlefish_views-20170406-20220407.csv")

#date has to be formatted and named "ds", word count named "y" to work for prophet function
df$Date <- as.Date(df$Date , "%Y-%m-%d")
colnames(df)[1] <- "ds"
colnames(df)[2] <- "y"

m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
tail(future)

#forecast object is a dataframe with a column yhat containing the forecast
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) #show only these columns
plot(m, forecast) #plot forecast
prophet_plot_components(m, forecast) #break down forecast into trend, weekly seasonality and yearly seasonality
dyplot.prophet(m, forecast) #interactive plot
