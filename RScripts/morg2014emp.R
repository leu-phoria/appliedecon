pacman::p_load(data.table, ggplot2, rstudioapi, DataExplorer, bit64, boot, np, estimatr, skedastic)

setwd("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA"); getwd()
morgdata <- fread(paste0("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA/morg-2014-emp.csv"))
summary(morgdata) #overview of data
#Create a sample indicator (not subsetting!) for  1 = Market research analysts and marketing specialists; 2 =  Computer and Mathematical Occupations
#occupational index (computer+maths and market analysis ppl)
morgdata[,sample := ifelse(occ2012==0735,1, ifelse(morgdata$occ2012>=1005 & morgdata$occ2012<=1240,2,0))]
dt <- morgdata

#create female dummy and variables: wages, log wages,agesq
dt[,female:=as.numeric(sex==2)][ 
  ,w:=earnwke/uhours ][
    ,lnw:=log(w)][
      ,agesq:=age^2] 

# dt1 Market research analysts and marketing specialists 
# dt2 Computer and Mathematical Occupations
dt1 <- dt[sample == 1, ]
dt2 <- dt[sample == 2, ]

#data exploration
dim(dt2[w<1,]) #4 individuals who have wages <1
#Discard observations with w < 1.
dt2 <- dt2[w>=1, ]
summary(dt2[,c('uhours','earnwke','w')]) #summary of these columns

#dt2 overview quick scatterplot
quickplot(x=age, y=w, data=dt2)
quickplot(x=age, y=lnw, data=dt2)

#dt1 overview quick scatterplot
quickplot(x=age, y=w, data=dt1)
quickplot(x=age, y=lnw, data=dt1)

#local linear kernel density regression age and wage dt2
npplot(npregbw(xdat = dt2$age, ydat = dt2$w, regtype ='ll'), plot.errors.method = 'asymptotic',  plot.errors.style = 'band')
#local linear kernel density regression age and logwage dt2
npplot(npregbw(xdat = dt2$age, ydat = dt2$lnw, regtype ='ll'), plot.errors.method = 'asymptotic',  plot.errors.style = 'band')
points(x=dt2$age,y=dt2$lnw, cex=.7, col="steelblue")

#take a look at differences between female and male
ggplot(data = dt2, aes(x= as.factor(sex), y = w)) + geom_boxplot() #there is a difference between men(=1) and women(=2), women have more outliers
ggplot(data = dt2, aes(x= as.factor(sex), y = w)) + geom_violin() + geom_jitter() #there are way more male obs than female

dim(dt2[sex == 1,]) #3438 male obs
dim(dt2[sex == 2,]) #1298 female obs

#do some regressions
##wage and age
reg1 <- lm(w ~ age + agesq, data = dt2); summary(reg1) #linear regression
reg1_hrob <- lm_robust(w ~ age + agesq, data = dt2, se_type = 'HC1'); summary(reg1_hrob) #heterosc robust coefficients
##lnwage and age
reg2 <- lm_robust(lnw ~ age + agesq, data = dt2); summary(reg2) #lin reg with robust se
pred2 <- predict(lm(lnw ~ age + agesq, data = dt2), interval = 'confidence'); summary(pred2) #predict doesn't work with lm_robust, so we use lm. Confidence interval is different to robust!
plot(dt2$age, dt2$lnw, cex = .7)
lines(dt2$age, pred2[,1], lty=4, col= "red")
##lnwage and female + age
reg3 <- lm_robust(lnw ~ female, data = dt2, se_type = 'HC1'); summary(reg3) #female earns 14 percent less than men cp
reg3.1 <- lm_robust(lnw ~ age + agesq + female, data = dt2, se_type = 'HC1'); summary(reg3.1) #Rsq is higher than reg2

#subsetting data, where  >=20 work hours, weekly earnings >0, age 24-64, grade92 (education degree)
dim(dt2[uhours >= 20 & earnwke > 0 & age >= 24 & age <= 64 & grade92 >= 44,  ]) #1083 observations
#dim(dt2[uhours >= 20 & earnwke > 0 & age >= 24 & age <= 64 & ,  ]) 4496 obs
dt2 <- dt2[uhours >= 20 & earnwke > 0 & age >= 24 & age <= 64 & grade92 >= 44,  ]

##reg4 with interaction
reg4 <- lm_robust(lnw ~ age + female + female * age, data = dt2, se_type = 'HC1' ); summary(reg4) #y=ß0+ß1x1+ß2x2+ß3*x1*x2
#summary(lm_robust(lnw~ age + female, data=dt2, se_type='HC1'))
(intercept_fem <- coef(reg4)[1] + coef(reg4)[3]) #ß0+ß2 
(slope_fem <- coef(reg4)[2] + coef(reg4)[4]) #ß1+ß3
# females start off with a higher intercept in subsample, but the wage progress over age is flatter

##reg5 with agesq and 2 interactions
reg5 <- lm_robust(lnw ~ age + agesq + female + female * age + female * agesq, data = dt2, se_type = 'HC1'); summary(reg5)
#female, age:female and agesq:female are not significant, so we test a joint hypothesis on them
library(car)
linearHypothesis(reg5, c("female", "age:female", "agesq:female"), c(0,0,0)) #p-value 1.625e-05 ***, they are jointly significant

##Further variable definitions (create dummy variables)
#PhD/ educational status
dt2[ ,ed_MA := as.numeric(grade92==44)][ 
  ,ed_Profess := as.numeric(grade92 == 45)][ 
    ,ed_PhD := as.numeric(grade92 ==46)]
#marital status
dt2[ , married := as.numeric(marital ==1 | marital ==2)][
  , divorced := as.numeric(marital == 3 | marital == 5 | marital == 6)][
    , widowed := as.numeric(marital == 4)][
      , nevermar := as.numeric(marital == 7)]
dt[ , married := as.numeric(marital ==1 | marital ==2)][
  , divorced := as.numeric(marital == 3 | marital == 5 | marital == 6)][
    , widowed := as.numeric(marital == 4)][
      , nevermar := as.numeric(marital == 7)]
#race
dt2[,white:= as.numeric(race==1)][
  , afram := as.numeric(race==2)][ 
    , asam := as.numeric(4)][
      , hisp := !is.na(ethnic)][
        , other.nonw:=as.numeric(white==0&afram==0&asam ==0&hisp==0)]
dt[,white:= as.numeric(race==1)][
  , afram := as.numeric(race==2)][ 
    , asam := as.numeric(4)][
      , hisp := !is.na(ethnic)][
        , other.nonw:=as.numeric(white==0&afram==0&asam ==0&hisp==0)]

##Does educational status make a difference
reg6 <- lm_robust(lnw ~ age + agesq + female + ed_PhD + age*ed_PhD + agesq * ed_PhD, data = dt2, se_type = 'HC1'); summary(reg6)
#ed_PhD, age:ed_PhD, aegsqp:edPhD not signif, so test jointly again
linearHypothesis(reg6, c("ed_PhD", "age:ed_PhD", "agesq:ed_PhD"), c(0,0,0)) #jointly significant 0.0003376 ***
library(stargazer) #produces well-formatted tables that hold regression analysis results
stargazer(lm(lnw ~ age + agesq + female + ed_PhD + age*ed_PhD + agesq * ed_PhD, data = dt2, se_type = 'HC1'), type='text') #CAVE not robust reg version

reg7 <- lm_robust(lnw ~ age + agesq + female + ed_PhD, data = dt2, se_type = 'HC1'); summary(reg7)
##Does being white make a difference
reg8 <- lm_robust(lnw ~ age + agesq + female + white,data = dt2, se_type = 'HC1' ); summary(reg8) #white not significant for the subpop of Computer and Mathematical Occupations

#Same regressions in population
reg9 <- lm_robust(lnw ~ age + agesq + female + white,data = dt, se_type = 'HC1' ); summary(reg9) #white significant for whole population
reg10 <- lm_robust(lnw ~ age+ agesq + female + white + nevermar, data =dt, se_type = 'HC1'); summary(reg10); summary(reg10)
