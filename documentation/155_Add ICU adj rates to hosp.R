#now to make rates adjusted by ICU type
#get the weights first
ICU_d <- readRDS("ICU_d.rds")
nrow(ICU_d)
colnames(ICU_d)

#we have decided to use CAUTI as the DV
#so we'll just make CAUTI vars

#make sum of CAUTI Days per ICU
#(numerator of the weights)
wt1_CAUTIDays <-aggregate(as.numeric(ICU_d$CAUTIDays), by=list(ICU_d$OrgID, ICU_d$ICUGrp), 
	FUN=sum, na.rm=TRUE)
names(wt1_CAUTIDays) <- c("OrgID", "ICUGrp", "CAUTIDays_by_ICUGrp")

#now make a sum of CAUTI days per hospital total
#(denominator of the weights)
wt2_TotCAUTIDays <-aggregate(as.numeric(ICU_d$CAUTIDays), by=list(ICU_d$OrgID), 
	FUN=sum, na.rm=TRUE)
names(wt2_TotCAUTIDays) <- c("OrgID", "CAUTIDays_by_Hosp")

#left join wt2 to wt1 to get the total repeated on 
#every row of wt1

nrow(wt1_CAUTIDays)
wt3_AddTotal <- merge(x = wt1_CAUTIDays, y = wt2_TotCAUTIDays, by = "OrgID", all.x = TRUE)
nrow(wt3_AddTotal)

#create rates for each ICU stratum
#for each hospital
#first, calculate the stratum weight
wt3_AddTotal$StratumWt <- wt3_AddTotal$CAUTIDays_by_ICUGrp/wt3_AddTotal$CAUTIDays_by_Hosp

#but now we need the rates per ICU
#so let's do another aggregate 

#make sum of CAUTI cases per ICU
#(numerator of the ICU-level rates)
wt4_CAUTINumTot <-aggregate(as.numeric(ICU_d$CAUTINum), by=list(ICU_d$OrgID, ICU_d$ICUGrp), 
	FUN=sum, na.rm=TRUE)
names(wt4_CAUTINumTot) <- c("OrgID", "ICUGrp", "CAUTINumTot_by_ICUGrp")

#join onto our dataset
nrow(wt3_AddTotal)
wt5_AddNumerator <- merge(x = wt3_AddTotal, y = wt4_CAUTINumTot, by = c("OrgID", "ICUGrp"), 
	all.x = TRUE)
nrow(wt5_AddNumerator)
colnames(wt5_AddNumerator)

#calculate raw rates
wt5_AddNumerator$RawRate <- wt5_AddNumerator$CAUTINumTot_by_ICUGrp/wt5_AddNumerator$CAUTIDays_by_ICUGrp
#calcualte adjusted rates
wt5_AddNumerator$AdjRate <- wt5_AddNumerator$RawRate * wt5_AddNumerator$StratumWt

#export dataset so as not to lose it if I need it again
CAUTI_ICU_Rates_Wts <- wt5_AddNumerator
saveRDS(CAUTI_ICU_Rates_Wts, "CAUTI_ICU_Rates_Wts.rds")

#sum by hospital to get totally adjusted rate
wt5_HospAdjRate <-aggregate(as.numeric(wt5_AddNumerator$AdjRate), by=list(wt5_AddNumerator$OrgID), 
	FUN=sum, na.rm=TRUE)
names(wt5_HospAdjRate) <- c("OrgID", "CAUTIRateAdj")

#left join to hosp_d - our most recent hospital dataset
hosp_d <- readRDS("hosp_d.rds")

nrow(hosp_d)
nrow(wt5_HospAdjRate)
hosp_e <- merge(x = hosp_d, y = wt5_HospAdjRate, by = "OrgID", all.x = TRUE)
nrow(hosp_e)

#export
saveRDS(hosp_e, "hosp_e.rds")

