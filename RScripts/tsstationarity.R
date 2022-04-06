x0 <- 0
y0 <- 0

n <- 10000

x <- rep(NaN, n)
y <- rep(NaN, n)

x[1] <- x0 + rnorm(1,mean = 0, sd = 2)


epsy <- rnorm(n, mean = 0, sd = 1)
y[1] <- y0 + epsy[1]

for(t in 2:n){
  x[t] <- x[t-1] + rnorm(1,0,2)
}

for(t in 2:n){
  y[t] <- y[t-1] + epsy[t]
}

#two random walks
plot(x, type = 'l', col = 'steelblue', lwd = 4 )
lines(y, type='l', col = 'black', lwd = 4)

##they have nothing to do with each other but we still regress, coeff are high and signif
spurious_reg <- lm(y~x); summary(spurious_reg) 
spurious_reg2 <- lm(x~y); summary(spurious_reg2)

library(urca) #for secgen testing
#ers - Elliott Rothenburg Test
#Null hypothesis: series non-stationary! One-sided test. 
summary(ur.ers(x))
#test statistic is greater than critical values -> can not reject H0 at 10/5/1 pct level

#Null hypothesis: series stationary
summary(ur.kpss(x))
#test statistic is larger than all critical values -> reject H0, process is stationary

library(xts)
library(lmtest)

x <- as.ts(x) #define x as timeseries
dx <- diff(x) #take the first difference, change in x
plot(dx, type='l', col='steelblue', lwd=4) # this is how stationary processes should look like (eg white noise)
y <- as.ts(y)
dy <- diff(y)

#Test Granger causality in both directions.
#Null hypothesis: Granger non-causality.
grangertest(x,y, order = 1) #specify order=1 because we created AR1 process
#Model 1 including xt, model 2 without. 
#test result is not statistically signif -> cannot reject H0.
grangertest(y,x, order = 1)
