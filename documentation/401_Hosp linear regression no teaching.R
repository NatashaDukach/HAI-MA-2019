hosp_analytic <- readRDS("hospital_analytic.rds")
colnames(hosp_analytic)

#remove those without outcome
nrow(hosp_analytic)
hosp <- subset(hosp_analytic, !is.na(CAUTIRateAdj))
nrow(hosp)

#linear models without Teaching
modelA <- lm(CAUTIRateAdj ~ Adm2 + Adm3 + PropNonwhite, data = hosp)
summary(modelA)