library(np) 
library(ggplot2)

data(cps71)
attach(cps71)

#Non parametric regressions
#Create transformed variables.
age2 <- age^2
age3 <- age^3
age4 <- age^4
age5 <- age^5
l_age <- log(age)
sdage <- sd(age)

param1 <- lm(logwage ~ age, x=T, y=T); summary(param1)
param2 <- lm(logwage ~ age + age2, x=TRUE, y=TRUE); summary(param2)
param3 <- lm(logwage ~ age + age2 +age3, x=TRUE, y=TRUE); summary(param3) #cubic function
param4 <- lm(logwage ~ age + age2+age3+age4, x=TRUE, y=TRUE); summary(param4) #better R squared, but understanding the coefficients becomes more complicated
param5 <- lm(logwage ~ age + age2 + age3 + age4 + age5, x=TRUE, y=TRUE); summary(param5) #shows overfitting, Rsq gets better, but statistics less signif
paramlog <- lm(logwage ~ age+ l_age, x=TRUE, y=TRUE);summary(paramlog)
plot(age, logwage, xlab="Age", ylab="Log wage", cex=.7, ylim=c(11,15.1)) #could be quadratic, but also cubic

#Fitted curve with confidence intervals
pred1 <- predict(param1, interval="confidence") 
pred2 <- predict(param2, interval="confidence")
pred3 <- predict(param3, interval="confidence")
pred4 <- predict(param4, interval="confidence")
pred5 <- predict(param5, interval="confidence")

# Plot regression lines with confidence intervals.

plot(age, logwage, xlab="Age", ylab="Log wage", cex=.7, ylim=c(11,15.1))
matlines(age,pred1,lty=c(1,2,2), col=c("black","black","black"))

plot(age, logwage, xlab="Age", ylab="Log wage", cex=.7, ylim=c(11,15.1)) #quadratic
matlines(age,pred2,lty=c(1,2,2), col=c("black","black","black"))

plot(age, logwage, xlab="Age", ylab="Log wage", cex=.7, ylim=c(11,15.1)) #cubic
matlines(age,pred3,lty=c(1,2,2), col=c("black","black","black"))

plot(age, logwage, xlab="Age", ylab="Log wage", cex=.7, ylim=c(11,15.1)) #quartic
matlines(age,pred4,lty=c(1,2,2), col=c("black","black","black"))

plot(age, logwage, xlab="Age", ylab="Log wage", cex=.7, ylim=c(11,15.1)) #fifth power
matlines(age,pred5,lty=c(1,2,2), col=c("black","black","black"))

##### 
# Non-parametric regression

#npregbw estimates a non-prametric regression line with good (optimal) bandwidth using a default bandwidth selection method.
#npplot plots the results with asymptotic confidence bands around the estimated values

#Local constant kernel regression (Nadaraya-Watson)

npplot(npregbw(xdat=age, ydat=logwage,regtype="lc", bwmethog="cv.ls"), plot.errors.method="asymptotic", ylim=c(11,15.1),plot.errors.style="band")
points(x=age,y=logwage, cex=.7, col="steelblue")
abline(v=median(age), lty=3)

#Local linear kernel regression -- summary: better bahvior at the boundary of the data.

npplot(npregbw(xdat=age, ydat=logwage,regtype="ll"), plot.errors.method="asymptotic", ylim=c(11,15.1),plot.errors.style="band")
points(x=age,y=logwage, cex=.7, col="steelblue")

detach(cps71)
