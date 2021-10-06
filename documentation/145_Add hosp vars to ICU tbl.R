#Patch teaching status from Hosp table
#onto ICU table
hosp <- readRDS("hosp_c.rds")
colnames(hosp)
ICU_c <- readRDS("ICU_c.rds")
ICU_c$CAUTIDays

#keep columns from hosp that we want to patch onto ICU
keep <- c("OrgID", "MedAfilGrp", "MedAfilFlag", "HospTypeID", "Teaching", "TeachFlag",
		"ProfitID", "ProfitFlag", "HospPerfScore", "PerfScoreCat", "TPSQ1")
#hosp_keep is the datafram with just the hospital variables
#I want to add to the ICU table so we can have them also at that level
hosp_keep <- hosp[keep]
nrow(ICU_c)
ICU_d <- merge(x = ICU_c, y = hosp_keep, by = "OrgID", all.x = TRUE)
nrow(ICU_d)
colnames(ICU_d)

#export
saveRDS(ICU_d, "ICU_d.rds")