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
