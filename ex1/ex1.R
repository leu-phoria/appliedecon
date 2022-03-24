setwd("~/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/ex1")

df <- read.csv("anscombe.csv", header=T, sep=",")

df[2,3] #value in 2nd row, 3rd column

head(df)

df[1:3,]

1:3
#same as
seq(1,3, by=1)

summary(df)

pinkpony <- c(1,3,5,8,9) #vector
mean(pinkpony) #mean of vector
pinkpony+2 #adding 2 to every value in vector
pinkpony+pinkpony+pinkpony
length(pinkpony)

summary(pinkpony)

bluepony <- c(2,5,7,9,22)

pony <- cbind(pinkpony,bluepony) #combines these two column wise

ponytail <- c(pinkpony, bluepony)
ponytail #combines in a row

#define a function, gives me first element of a and first element of b
first_element <- function(a,b){
  u <- c(NA,NA)
  u[1] <- a[1]
  u[2] <- b[1]
  return(u)
}
first_element(pinkpony, bluepony)

#for loops
multiplepony <- rep(NA, length(pinkpony)) 

for (i in 1: length(pinkpony)) {
  multiplepony[i] <- pinkpony [i]* bluepony[i]
}

multiplepony

plot(x= df$x1, y=df$y1)
abline( a=3, b=0.5, lwd=4, col="red") #a intercept, b slope

plot (x=df$x2, y=df$y2)
abline( a=3, b=0.5, lwd=4, col="green")

plot (x=df$x3, y=df$y3)
abline( a=3, b=0.5, lwd=4, col="green")

plot (x=df$x4, y=df$y4)
abline( a=3, b=0.5, lwd=4, col="green")

coef(lm(y2~x2, data=df)) #linear regression model

ponydata <- c(1,2,3,NA,5)
sum(ponydata, na.rm=T) #remove NA= true
