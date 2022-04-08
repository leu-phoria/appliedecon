library(plm) 
data("Grunfeld") 

head(Grunfeld) #how much firms invest, their value & capital
summary(Grunfeld)

#fixed effects, demeaning
fe <- plm(inv~ value + capital, data =Grunfeld, model="within") #within --> demean estimation
summary(fe)

#random effect
re <- plm(inv~ value + capital, data =Grunfeld, model="random")
summary(re)

##fe and re estimates and se are quite similar --> Hausman test
##no intercept in fe, but re and fe estim nevertheless the same

phtest(inv~ value + capital, data =Grunfeld) 
#p value 0.3119, cannot reject H0: random effects
#with large datasets rather expected to reject H0

#first difference
fd <- plm(inv~ value + capital, data =Grunfeld, model="fd")
summary(fd)

