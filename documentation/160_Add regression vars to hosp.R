#read in current hospital table
hosp_e <- readRDS("hosp_e.rds")
colnames(hosp_e)

#add some final regression variables
hosp_e$CAH <- 0
CAH_list <- c("13262", "12833", "13189")
hosp_e$CAH[hosp_e$OrgID %in% CAH_list] <- 1
table(hosp_e$CAH)

hosp_e$NonUrban <- 0
NonUrban_County_List <- c("Dukes", "Barnstable", "Nantucket", "Franklin", "Berkshire")
hosp_e$NonUrban[hosp_e$HospCountyName_c %in% NonUrban_County_List] <- 1
table(hosp_e$NonUrban)

table(hosp_e$NonUrban, hosp_e$CAH)

#make quartiles for CAUTI DV
CAUTI_quant <- as.data.frame(quantile(hosp_e$CAUTIRateAdj, na.rm = TRUE))
cutpoint_for_top_quartile <- CAUTI_quant[4,1]
hosp_e$CAUTIQ4 <- 0
hosp_e$CAUTIQ4[hosp_e$CAUTIRateAdj >= cutpoint_for_top_quartile] <- 1
table(hosp_e$CAUTIQ4, hosp_e$CAUTIRateAdj)
table(hosp_e$CAUTIQ4)
table(hosp_e$CAUTIQ4, hosp_e$Teaching)

#make analytic dataset

# hospital_analytic <- hosp_e
# saveRDS(hospital_analytic, "hospital_analytic.rds")

# INSERT FOR THE NEW MODEL AND SCORING ALGORYTHM

hosp_e$Fac1Score <- hosp_e$H9Beds + hosp_e$H9ICUBeds + hosp_e$H9NumAdm + hosp_e$H9NumPatDays
hosp_e$Fac2Score <- hosp_e$Prop65Plus + hosp_e$NonUrban

hosp_e$RegStatus <- 4
hosp_e$RegStatus[hosp_e$CAUTICases == 0] <- 1 # 21 observations with no CAUTI cases in the original data were set to 0 
hosp_e$RegStatus[hosp_e$CAUTICases > 0 & hosp_e$CAUTICases <= 4 ] <- 2 # why less than 4 CAUTI Cases?
hosp_e$HospPerfScoreFlag <- as.character(is.na(hosp_e$HospPerfScore))
hosp_e$RegStatus[hosp_e$RegStatus > 2 & hosp_e$HospPerfScoreFlag == c("TRUE")] <- 3
table(hosp_e$RegStatus)


# sub <- hosp_e[hosp_e$CAUTICases>0 & !is.na(hosp_e$CAUTICases) &!is.na(hosp_e$HospPerfScore), ] # 39 hospitals 
# # hospitals with NA for CAUTI cases were set to 0
# 
# quantile(sub$CAUTICases)
# hist(sub$CAUTICases, breaks = 20)
# table(cut(sub$CAUTICases, quantile(sub$CAUTICases, 0:3 / 3)))
# table(cut(sub$CAUTICases, seq(min(sub$CAUTICases), max(sub$CAUTICases), length = 10 + 1))) # deciles
# 
# Regression_Model <- lm(CAUTIRateCrude ~ HospPerfScore + Fac1Score + Fac2Score + PropNonwhite, data = sub)
# summary(Regression_Model)

#This is our final specified model - we are using the CAUTI crude rate
#and only those hospitals eligible for the regression
Regression_Model <- lm(CAUTIRateCrude ~ HospPerfScore + Fac1Score + Fac2Score + PropNonwhite, data = Regression_df)
summary(Regression_Model)

intercept <- c(-0.0003843)
HospPerfScore_Slope <- c(0.000001461)
Fac1Score_Slope <- c(0.000000001634)
Fac2Score_Slope <- c(0.0012)
PropNonwhite_Slope <- c(0.00358)

hosp_e$yhat <- intercept + (HospPerfScore_Slope* hosp_e$HospPerfScore) +
  (Fac1Score_Slope * hosp_e$Fac1Score) +
  (Fac2Score_Slope * hosp_e$Fac2Score) +
  (PropNonwhite_Slope * hosp_e$PropNonwhite)

class(hosp_e$CAUTIRateCrude)
class(hosp_e$yhat)

#CLEAN UP ALGORITHM

#Set yhat to crude where there are zero CAUTI cases
hosp_e$yhat[hosp_e$RegStatus == 1] <- hosp_e$CAUTIRateCrude[hosp_e$RegStatus == 1]
#set yhat to crude where there is no TPS
hosp_e$yhat[hosp_e$HospPerfScoreFlag == "TRUE"] <- hosp_e$CAUTIRateCrude[hosp_e$HospPerfScoreFlag == "TRUE"]

#Set yhat to 0 where there are NA CAUTICases
hosp_e$CAUTICasesFlag <- as.character(is.na(hosp_e$CAUTICases))
hosp_e$yhat[hosp_e$CAUTICasesFlag == c("TRUE")] <- 0

#Set yhat to CAUTIRateCrude for these few NA hospitals
#ORGIDs 
#12618 (6 cases)
#13116 (7 cases)
#13201 (5 cases)

class(hosp_e$OrgID)
hosp_e$yhat[hosp_e$OrgID %in% c(12618, 13116, 13201)] <- hosp_e$CAUTIRateCrude[hosp_e$OrgID %in% c(12618, 13116, 13201)]

hosp_e$resid <- hosp_e$CAUTIRateCrude - hosp_e$yhat
hist(hosp_e$resid)
hosp_e[c("OrgID", "CAUTICases", "CAUTIRateCrude", "yhat", "resid", "HospPerfScore", "Fac1Score", "Fac2Score", "PropNonwhite")]

#code tertiles above zero
nrow(hosp_e)
nonzero <- subset(hosp_e, yhat != 0)
nrow(nonzero)
yhat_tertiles <- quantile(nonzero$yhat, c(0:3/3))

A <- yhat_tertiles[1]
B <- yhat_tertiles[2]
C <- yhat_tertiles[3]

hosp_e$Quartile <- 9
hosp_e$Quartile[hosp_e$yhat < A] <- 1
hosp_e$Quartile[hosp_e$yhat >= A & hosp_e$yhat < B] <- 2
hosp_e$Quartile[hosp_e$yhat >= B & hosp_e$yhat < C] <- 3
hosp_e$Quartile[hosp_e$yhat >= C] <- 4
table(hosp_e$yhat, hosp_e$Quartile)
table(hosp_e$Quartile)

hosp_e[c("resid", "CAUTIRateCrude", "HospPerfScore", "yhat", "Quartile", "CAUTICases")]
table(hosp_e$HospPerfScoreFlag)

#Examine out the quantiles on the residual
resid_quant <- quantile(hosp_e$resid, na.rm = TRUE)
big_resid <- subset(hosp_e, resid > resid_quant[3])
big_resid[c("resid", "CAUTIRateCrude", "HospPerfScore", "yhat", "Quartile", "CAUTICases")]
small_resid <- subset(hosp_e, resid < resid_quant[2])
small_resid[c("resid", "CAUTIRateCrude", "HospPerfScore", "yhat", "Quartile", "CAUTICases")]

#make analytic dataset
hospital_analytic <- hosp_e
saveRDS(hospital_analytic, "hospital_analytic.rds")

nrow(hospital_analytic)

#Make regression df for those eligible
nrow(hosp_e)
Regression_df <- subset(hosp_e, RegStatus == 4 | RegStatus == 1)
nrow(Regression_df)
table(Regression_df$RegStatus)

