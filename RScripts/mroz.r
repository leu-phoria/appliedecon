pacman::p_load(mfx, wooldridge)
#mfx helps to analyse avg partial effects
library(estimatr) #for robust estimators
library(stargazer)
data(mroz)

dim(mroz)

summary(mroz) #https://www.rdocumentation.org/packages/npsf/versions/0.8.0/topics/mroz for description of var

# inlf 1 if in labor force, (faminc - wage*hours)/1000, actual labor mkt exper, #kids below 6 and 6-18
lpm <- lm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, data = mroz); summary(lpm) #linear predictive model
#don't interpret std. errors, because they should be robust
#stargazer(lpm, title="Results", align=TRUE, type='text')

model <- inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6

#robust estimators
lpm_robust <- lm_robust(model, data = mroz, se_type = 'HC1'); summary(lpm_robust)

#logit model
logit <- glm(model, data = mroz, family = binomial(link="logit")); summary(logit)
#probit model
probit <- glm(model, data = mroz, family = binomial(link="probit")); summary(probit)

# coef(probit)
# coef(probit)*1.6 # probit*1.6 similar magnitudes as logit
# coef(logit)

logitmfx(model, data = mroz) #marginal effects of logit model
#interpretation: 1 more year in education, provides on avg 5percent increase in probability of being employed
probitmfx(model, data = mroz)#marginal effects of probit model

#create a profile: intercept, mean nwifeinc,17y educ, 10yrs exper, mean expersq, age 45, 0 kids under 6, 1 kid over 6
profile <- c(1, 20, 17, 10, 100, 45, 0, 1)

#logit estimation
#use coef of logit function and plug profile into it to calculate probability of participation in labour force
# %*% matrix manipulation profil*coeff from logit
(singleindex <- profile %*% coef(logit)) #1.600745 Linear index (auf x-Achse)
plogis(singleindex) #probability of participation in labour market (probability of logistic function)
dlogis(singleindex) * coef(logit)[3] #marginal effect of 1 more year education on probability of participation in labour market (density of logistic function)

#probit estimation
(singleindex_p <- profile %*% coef(probit)) #linear index
pnorm(singleindex_p) #probability of participation in labour market
dnorm(singleindex_p) * coef(probit)[3] #marginal effect of 1 more year education on probability of participation in labour market

summary(lpm)c

##Classification using threshold 0.5. If predicted probability is >= 0.5, then classify 1, if <, classify 0.
#Proportion of correctly classified individuals.