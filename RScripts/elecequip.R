library(fpp2) #contains elecequip data
library(seasonal)
#elecequip dataset: the number of new orders for electrical equipment (computer, electronic and optical products) in the Eurozone area (16 countries/ eurostat)
data(elecequip)
fit <- stl(elecequip, s.window=7) #seasonal decomposition of time series data

autoplot(fit) + xlab("Year") # f(st, Tt, Rt) - Remainder = data - trend - seasonal, Remainder looks stationary

library(aTSA) #Philipps Peron test

fit
pp.test(fit$time.series[,'remainder']) #p value is small, we can reject H0 and accept that it's a stationary process
#adf.test(fit$time.series[,'remainder'])

tsy <- fit$time.series[,'remainder']
acf(tsy) #autocorrelation function. today (left vertical) only correlated with the last three time periods, but not further 
pacf(tsy) #partial autocorr function - autocorrelation between today and 2 time periods ago, partialling out yesterday
autoplot(tsy) #looks stationary

library(forecast) #for ARMA

#we analyse the remainders as time series
#ARMA process
armay <-  auto.arima(tsy, stationary=T, seasonal= F)
summary(armay) #it automatically selected 1p and 1q

#AR process
ary <-  auto.arima(tsy, max.q = 0, stationary = T, seasonal = F) #Maximum value of q
summary(ary) #best AR model if MA suppressed is with 4 lags

#MA process
may <- auto.arima(tsy, max.p = 0, stationary = T, seasonal = F)
summary(may) #best MA model with 3 lags

#not the remainders anymore, analysis of elecequip data
arima.elecequip <- auto.arima(elecequip)
summary(arima.elecequip) #4 AR, 1 MA, 1 seasonal component

plot(ary) #4 points within circle, if not --> unit root or explosive process
plot(armay) #stable ARMA estimate, points within circle

#illustrate forecasting
#autoplot(forecast(armay)) did not work bc of 2 forecast versions
#autoplot from forecast library
autoplot(forecast::forecast(armay)) 
autoplot(forecast::forecast(arima.elecequip)) #h determines forecast time into future, 
autoplot(forecast::forecast(arima.elecequip, h=1)) #usually stick to h=1-3


