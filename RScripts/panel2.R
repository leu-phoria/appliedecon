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
#p value 0.3119, cannot reject H0. random effects
#with large datasets rather expected to reject H0

#first difference
fd <- plm(inv~ value + capital, data =Grunfeld, model="fd")
summary(fd)

#data empluk
data("EmplUK")
summary(EmplUK)
#plot(EmplUK$emp, type ='l')
#panel GMM 
dynpal <- pgmm(log(emp)~lag(log(emp), 1:2) + lag(log(wage), 0:1)+ log(capital)+ #1:2 first and second lag of emp, time 0 and t-1 logwage
               lag(log(output), 0:1) | lag(log(emp),2:99), data=EmplUK, # | log(emp) --> instrument with all lags, that we didn't include
               effect= "individual", model = "twosteps") 
summary(dynpal)
#interpretation is not so easy (?!) - some signif effects