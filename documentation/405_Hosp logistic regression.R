hosp_analytic <- readRDS("hospital_analytic.rds")
colnames(hosp_analytic)

#remove those without outcome
nrow(hosp_analytic)
hosp <- subset(hosp_analytic, !is.na(CAUTIRateAdj))
nrow(hosp)
colnames(hosp)

Model11 <- glm(CAUTIQ4 ~ Teaching, data = hosp, family = "binomial")
summary(Model11)

Model12 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3, data = hosp, family = "binomial")
summary(Model12)

Model13 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3 + PatDay2 + PatDay3, data = hosp, family = "binomial")
summary(Model13)

Model14 <- glm(CAUTIQ4 ~ Teaching + H9NumAdm, data = hosp, family = "binomial")
summary(Model14)

Model15 <- glm(CAUTIQ4 ~ Teaching + H9NumAdm + PatDay2 + PatDay3, data = hosp, family = "binomial")
summary(Model15)

Model16 <- glm(CAUTIQ4 ~ Teaching + H9NumAdm + H9NumPatDays, data = hosp, family = "binomial")
summary(Model16)

Model17 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3 + H9NumPatDays, data = hosp, family = "binomial")
summary(Model17)

Model18 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3 + H9NumPatDays + ProfitFlag, data = hosp, family = "binomial")
summary(Model18)

Model19 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3 + H9NumPatDays + H9Beds, data = hosp, family = "binomial")
summary(Model19)

Model110 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3 + H9NumPatDays + Bed2 + Bed3, data = hosp, family = "binomial")
summary(Model110)

Model111 <- glm(CAUTIQ4 ~ Teaching + PatDay2 + PatDay3, data = hosp, family = "binomial")
summary(Model111)

Model112 <- glm(CAUTIQ4 ~ Teaching + Bed2 + Bed3, data = hosp, family = "binomial")
summary(Model112)

Model113 <- glm(CAUTIQ4 ~ Teaching + ICUBed3 + ICUBed4, data = hosp, family = "binomial")
summary(Model113)

Model114 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3 + ICUBed4, data = hosp, family = "binomial")
summary(Model114)

Model115 <- glm(CAUTIQ4 ~ Teaching + Adm2 + Adm3 + H9ICUBeds, data = hosp, family = "binomial")
summary(Model115)

Model116 <- glm(CAUTIQ4 ~ Teaching + Adm2 + H9ICUBeds, data = hosp, family = "binomial")
summary(Model116)

Model117 <- glm(CAUTIQ4 ~ Teaching + H9NumAdm + H9ICUBeds, data = hosp, family = "binomial")
summary(Model117)

Model118 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays + H9ICUBeds, data = hosp, family = "binomial")
summary(Model118)

Model119 <- glm(CAUTIQ4 ~ Teaching + H9ICUBeds, data = hosp, family = "binomial")
summary(Model119)

Model120 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays, data = hosp, family = "binomial")
summary(Model120)

Model121 <- glm(CAUTIQ4 ~ Teaching + H9Beds, data = hosp, family = "binomial")
summary(Model121)

Model122 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays + TPSQ1, data = hosp, family = "binomial")
summary(Model122)

Model123 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays + Pop2019, data = hosp, family = "binomial")
summary(Model123)

Model124 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays + Prop65Plus, data = hosp, family = "binomial")
summary(Model124)

Model125 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays + PropNonwhite, data = hosp, family = "binomial")
summary(Model125)

Model126 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays + PropPoverty, data = hosp, family = "binomial")
summary(Model126)

Model127 <- glm(CAUTIQ4 ~ Teaching + H9NumPatDays + NonUrban, data = hosp, family = "binomial")
summary(Model127)



#We think we have to live with the base model which is:
final_monitored <- glm(TargetLike ~ StudyID + Task6 + Task7 + Task8 + Singular + Plural + Modified, data = monitored, family = "binomial")
summary(final_monitored)

library (devtools)
library (broom)

Tidy_Monitored <- tidy(final_monitored)
Tidy_Monitored 

#Add calculations

Tidy_Monitored$OR <- exp(Tidy_Monitored$estimate)
Tidy_Monitored$LL <- exp(Tidy_Monitored$estimate - (1.96 * Tidy_Monitored$std.error))
Tidy_Monitored$UL <- exp(Tidy_Monitored$estimate + (1.96 * Tidy_Monitored$std.error))
Tidy_Monitored

write.csv(Tidy_Monitored, file = "Tidy_Monitored.csv")