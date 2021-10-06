hosp_analytic <- readRDS("hospital_analytic.rds")
colnames(hosp_analytic)

#remove those without outcome
nrow(hosp_analytic)
hosp <- subset(hosp_analytic, !is.na(CAUTIRateAdj))
nrow(hosp)

#first we will make a saturated model with all possible covariates
#and then do the backwards elimination

#library(leaps)
#library(car)
#library(tidyverse)
#library(broom)
#library(tibble)
#library(devtools)

#Model1 <- regsubsets(CAUTIRateAdj ~ TeachFlag + ProfitFlag + H9Beds + H9ICUBeds + H9NumPatDays +
			HospPerfScore + Pop2019 + Prop65Plus + PropNonwhite + NonUrban, data=hosp, nbest = 10)
#summary(Model1)
#plot(Model1, scale = "r2")
#subsets(Model1, statistic="rsq")

#I want to try factor analysis
#need to replace NAs in HospPerfScore

hosp$HospPerfScore[is.na(hosp$HospPerfScore)] <- 0
table(hosp$HospPerfScore, useNA = c("always"))

#all continuous

Vars1 <- c("TeachFlag", "ProfitFlag", "H9Beds", "H9ICUBeds", "H9NumAdm", 
		"H9NumPatDays", "HospPerfScore", "Pop2019", "Prop65Plus", "PropNonwhite", "NonUrban")
data1 <- hosp[Vars1]

fit1 <- princomp(data1, cor=TRUE)
summary(fit1) # print variance accounted for
loadings(fit1) # pc loadings
plot(fit1,type="lines") # scree plot
fit1$scores # the principal components
biplot(fit1)

library(psych)
fit1 <- principal(data1, nfactors=3, rotate="varimax")
fit1 # print results
