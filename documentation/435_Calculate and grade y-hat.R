hosp_analytic <- readRDS("hospital_analytic.rds")
colnames(hosp_analytic)

#remove those without outcome
nrow(hosp_analytic)
hosp <- subset(hosp_analytic, !is.na(CAUTIRateAdj))
nrow(hosp)

#need to turn HospPerfScore nulls to 0
#this is fine - 0 will cause the slope to drop out in theory
#this is how we modeled it
hosp$HospPerfScore[is.na(hosp$HospPerfScore)] <- 0
table(hosp$HospPerfScore, useNA = c("always"))

#Create factor scores
hosp$Fac1Score <- hosp$H9Beds + hosp$H9ICUBeds + hosp$H9NumAdm + hosp$H9NumPatDays
hosp$Fac2Score <- hosp$Prop65Plus + hosp$NonUrban

# y-hat calculated on the first linear model that included HospPerfScore from the file 430_Final Specified Models

final_specified3 <- lm(CAUTIRateAdj ~ HospPerfScore + Fac1Score + Fac2Score + PropNonwhite, data = hosp)
summary(final_specified3)


# (old linear model was used below for intercepts: final_specified3 )

intercept <- c(0.00003992)
HospPerfScore_Slope <- c(-0.000002844)
Fac1Score_Slope <- c(0.000000001755)
Fac2Score_Slope <- c(0.0005698)
PropNonwhite_Slope <- c(0.002827)

hosp$yhat <- intercept + (HospPerfScore_Slope* hosp$HospPerfScore) +
			(Fac1Score_Slope * hosp$Fac1Score) +
			(Fac2Score_Slope * hosp$Fac2Score) +
			(PropNonwhite_Slope * hosp$PropNonwhite)

#create yhat deciles and give them a grade on a curve!
#yhat_hexiles <- quantile(hosp$yhat, probs = seq(0, 1, 0.20))
yhat_deciles <- quantile(hosp$yhat, probs = seq(0, 1, 0.10))

Below_is_D <- yhat_deciles[5]
Below_is_C <- yhat_deciles[4]
Below_is_B <- yhat_deciles[3]
Below_is_A <- yhat_deciles[2]

hosp$grade <- c("Z")
hosp$grade[hosp$yhat <= Below_is_A] <- c("A")
hosp$grade[hosp$yhat > Below_is_A & hosp$yhat <= Below_is_B] <- c("B")
hosp$grade[hosp$yhat > Below_is_B & hosp$yhat <= Below_is_C] <- c("C")
hosp$grade[hosp$yhat > Below_is_C & hosp$yhat <= Below_is_D] <- c("D")
hosp$grade[hosp$yhat > Below_is_D] <- c("F")
table(hosp$grade, useNA = c("always"))
table(hosp$grade, hosp$Teaching)

#let's look at the top hospitals
Top <- subset(hosp, grade %in% c("A", "B"))
Top[c("H9Name", "grade", "CAUTIRateAdj", "yhat")]

#worst
Worst <- subset(hosp, grade %in% c("F"))
Worst[c("H9Name", "CAUTIRateAdj", "yhat")]

# THE Y-HAT MAKES NO SENSE IN THE ABOVE PRINTOUT - LOOK AT : 
# Salem Hospital - North Shore Medical Center - Salem, MA     GRADE A , CAUTI RATE - 0.0010184781 AND Y-HAT=0.0004935097
# same for many other hospitals

#export
hosp_graded <- hosp
saveRDS(hosp_graded, "hosp_graded.rds")
graded <- readRDS("hosp_graded.rds")
write.csv(graded,"hosp_graded.csv")


