pacman::p_load(data.table, ggplot2, rstudioapi, estimatr, tidyverse, huxtable)

data_panel <- read_csv(paste("/Users/Leu/Documents/BWL/WiSe 21:22/HU-WS21:22/Applied Econ/Exercises/AppliedEcon/DATA/worldbank-immunization-panel.csv", sep = "/"))

data_panel <- data_panel %>%
  filter(!(is.na(imm) | is.na(gdppc))) %>%
  mutate(c = factor(c)) %>%
  group_by(c) %>%
  mutate(balanced = min(year) == 1998 & max(year) == 2017 & length(unique(year)) == 20) %>%
  ungroup() 

data_balanced <- data_panel %>%
  filter(balanced == TRUE)

data_balanced <- data_balanced %>%
  arrange(c, year) %>%
  group_by(c) %>%
  mutate(
    lnpop=log(pop),
    d_surv = surv- lag(surv),
    d_imm = imm - lag(imm),
    d2_imm = d_imm - lag(d_imm), 
    d_lngdppc= lngdppc- lag(lngdppc),
    d_lnpop = lnpop - lag(lnpop),
    avgpop = mean(pop), #for weights in xtreg fe
    year = factor(year)
  ) %>%
  ungroup()

head(data_panel)
head(data_balanced)
  
#fixed effects

#de-mean estimator
fe_lm <- lm_robust(surv ~ imm + year,
                   data= data_balanced,
                   se_type = "stata",
                   fixed_effects = ~ c, #countries as fixed
                   clusters = c) #here we could also cluster on regional instead of national level for bigger cluster
summary(fe_lm) #two way fixed effects, we are not sure if effects constant over time, created dummy var for each year
#1998 left out because of perfect multicollinearity, it is the reference category
#if i take a given year, if imm rate is 10pct points higher than the avg of the country, then child survival tends to be 0.06 pc points higher than the country's avg within that year

fe_lm2 <- lm_robust(surv ~ imm + year + lngdppc + lnpop,
                    data = data_balanced,
                    se_type = "stata",
                    fixed_effects = ~ c,
                    clusters = c)
summary(fe_lm2) #lngdppc + lnpop significant, year not anymore, imm highly signif

#not cluster the se
fe_lm_nc <- lm_robust(surv ~ imm + year + lngdppc + lnpop,
                      data = data_balanced,
                      se_type = "stata",
                      fixed_effects = ~ c)
summary(fe_lm_nc) #we underestimate the se, t is higher, p val is lower, confidence intervals become more narrow 
## warning, don't ignore clustering

#first difference regression
fd_lm <- lm_robust(d_surv~ 0+ d_imm,
                   data = data_balanced,
                   se_type="stata",
                   clusters = c)
summary(fd_lm)
#if i increase immmunization by 10 pc (not compared to the avg!), then the survival rate will increase by 0.06pc points. direct interpretation