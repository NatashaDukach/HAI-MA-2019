hosp_analytic <- readRDS("hospital_analytic.rds")
colnames(hosp_analytic)

#remove those without outcome
nrow(hosp_analytic)
hosp <- subset(hosp_analytic, !is.na(CAUTIRateAdj))
nrow(hosp)

#make experimental ratio
hosp$PatDaysAdmRatio <- hosp$H9NumPatDays/hosp$H9NumAdm

#start modeling
model1 <- lm(CAUTIRateAdj ~ TeachFlag, data = hosp)
summary(model1)

model2 <- lm(CAUTIRateAdj ~ TeachFlag + NonUrban, data = hosp)
summary(model2)

model3 <- lm(CAUTIRateAdj ~ TeachFlag + ProfitFlag, data = hosp)
summary(model3)

model4 <- lm(CAUTIRateAdj ~ TeachFlag + H9ICUBeds, data = hosp)
summary(model4)

model5 <- lm(CAUTIRateAdj ~ TeachFlag + ICUBed3 + ICUBed4, data = hosp)
summary(model5)

#ICU bed used as measure of size
model6 <- lm(CAUTIRateAdj ~ TeachFlag + ICUBed4, data = hosp)
summary(model6)

#Cathdays for CAUTI as measure of utilization
model7 <- lm(CAUTIRateAdj ~ TeachFlag + ICUBed4 + CAUTICathdays, data = hosp)
summary(model7)

#This is not a good meausre of utilization
#try others
model8 <- lm(CAUTIRateAdj ~ TeachFlag + ICUBed4 + H9NumAdm, data = hosp)
summary(model8)

model9 <- lm(CAUTIRateAdj ~ TeachFlag + ICUBed4 + Adm2 + Adm3, data = hosp)
summary(model9)

model10 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3, data = hosp)
summary(model10)

model11 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + H9NumPatDays, data = hosp)
summary(model11)

model12 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + PatDay2 + PatDay3, data = hosp)
summary(model12)

model13 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + PatDay2 + PatDay3 + TPSQ1, data = hosp)
summary(model13)

model14 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + Prop65Plus, data = hosp)
summary(model14)

model15 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + PropNonwhite, data = hosp)
summary(model15)

model16 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + PropNonwhite + PropPoverty, data = hosp)
summary(model16)

model17 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + PropNonwhite + NonUrban, data = hosp)
summary(model17)

model18 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + PropNonwhite, data = hosp)
summary(model18)
#MODEL 18 IS ROUND 1 WORKING MODEL

model19 <- lm(CAUTIRateAdj ~ TeachFlag + Adm2 + Adm3 + PropNonwhite + H9NumPatDays, data = hosp)
summary(model19)

model20 <- lm(CAUTIRateAdj ~ TeachFlag + PropNonwhite + PatDaysAdmRatio, data = hosp)
summary(model20)








