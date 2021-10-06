hosp_analytic <- readRDS("hospital_analytic.rds")
colnames(hosp_analytic)

#remove those without outcome
nrow(hosp_analytic)
hosp <- subset(hosp_analytic, !is.na(CAUTIRateAdj))
nrow(hosp)

#Create factor scores
hosp$Fac1Score <- hosp$H9Beds + hosp$H9ICUBeds + hosp$H9NumAdm + hosp$H9NumPatDays
hosp$Fac2Score <- hosp$Prop65Plus + hosp$NonUrban

#final specified model
#but try each from Factor 3

#start modeling
final_specified1 <- lm(CAUTIRateAdj ~ TeachFlag + Fac1Score + Fac2Score + PropNonwhite, data = hosp)
summary(final_specified1)



final_specified2 <- lm(CAUTIRateAdj ~ ProfitFlag + Fac1Score + Fac2Score + PropNonwhite, data = hosp)
summary(final_specified2)

final_specified3 <- lm(CAUTIRateAdj ~ HospPerfScore + Fac1Score + Fac2Score + PropNonwhite, data = hosp)
summary(final_specified3)

#lets keep 3
#here we have logistic

final_specified1 <- glm(formula = CAUTIQ4 ~ TeachFlag + Fac1Score + Fac2Score + PropNonwhite, family = binomial, data = hosp)
summary(final_specified1)

#1st

final_specified2 <- glm(formula = CAUTIQ4 ~ ProfitFlag + Fac1Score + Fac2Score + PropNonwhite, family = binomial, data = hosp)
summary(final_specified2)

final_specified3 <- glm(formula = CAUTIQ4 ~ HospPerfScore + Fac1Score + Fac2Score + PropNonwhite, family = binomial, data = hosp)
summary(final_specified3)

#Again, 3 wins




