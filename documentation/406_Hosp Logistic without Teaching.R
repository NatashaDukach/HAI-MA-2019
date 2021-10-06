hosp_analytic <- readRDS("hospital_analytic.rds")
colnames(hosp_analytic)

#remove those without outcome
nrow(hosp_analytic)
hosp <- subset(hosp_analytic, !is.na(CAUTIRateAdj))
nrow(hosp)
colnames(hosp)

hosp$CAUTIRateAdj

#look at ORs without Teaching in the model
ModelA <- glm(CAUTIQ4 ~  H9NumAdm, data = hosp, family = "binomial")
summary(ModelA)

ModelB <- glm(CAUTIQ4 ~  H9NumPatDays, data = hosp, family = "binomial")
summary(ModelB)

ModelC <- glm(CAUTIQ4 ~  H9Beds, data = hosp, family = "binomial")
summary(ModelC)

ModelD <- glm(CAUTIQ4 ~  H9ICUBeds, data = hosp, family = "binomial")
summary(ModelD)