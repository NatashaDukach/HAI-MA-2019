#read in ICU_c and add vars
ICU_c <- readRDS("ICU_c.rds")
ICU_c$CAUTIDays
colnames(ICU_c)

#There are no NAs for CLABSIDays
ICU_c$NoCLABSIDays <- 0
ICU_c$NoCLABSIDays[ICU_c$CLABSIDays == 0] <- 1
table(ICU_c$CLABSIDays, ICU_c$NoCLABSIDays, useNA = c("always"))
#check data compliance with CLABSINum
table(ICU_c$CLABSINum, ICU_c$NoCLABSIDays, useNA = c("always"))
#All numerators are 0 where denominators are 0

#CAUTIDays - there were NAs in here
table(ICU_c$CAUTIDays, useNA = c("always"))
ICU_c$NoCAUTIDays <- 0
ICU_c$NoCAUTIDays[ICU_c$CAUTIDays == 0 | is.na(ICU_c$CAUTIDays)] <- 1
table(ICU_c$CAUTIDays, ICU_c$NoCAUTIDays, useNA = c("always"))
#check data compliance with CAUTINum
table(ICU_c$CAUTINum, ICU_c$NoCAUTIDays, useNA = c("always"))
#All numerators are 0 where denominators are 0 or NA
#make NA null to zero
ICU_c$CAUTIDays[is.na(ICU_c$CAUTIDays)] <- 0
ICU_c$CAUTINum[is.na(ICU_c$CAUTINum)] <- 0
table(ICU_c$CAUTINum, ICU_c$NoCAUTIDays, useNA = c("always"))

#Make setting-level rates
ICU_c$CLABSIRate <- as.numeric(ICU_c$CLABSINum)/as.numeric(ICU_c$CLABSIDays)
ICU_c$CAUTIRate <- as.numeric(ICU_c$CAUTINum)/as.numeric(ICU_c$CAUTIDays)


#Make ICUType designator
ICU_c$ICUTypeID <- 99
ICU_c$ICUTypeID[ICU_c$ICUType == "Burn ICU"] <- 1
ICU_c$ICUTypeID[ICU_c$ICUType == "Cardiac ICU"] <- 2
ICU_c$ICUTypeID[ICU_c$ICUType == "Cardiothoracic ICU"] <- 3
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical ICU (major teaching)"] <- 4
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical ICU (not major teaching)"] <- 5
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical Ward (major teaching)"] <- 6
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical Ward (not major teaching)"] <- 7
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical/Surgical ICU (major teaching)"] <- 8
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical/Surgical ICU (not major teaching)"] <- 9
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical/Surgical Ward (major teaching)"] <- 10
ICU_c$ICUTypeID[ICU_c$ICUType == "Medical/Surgical Ward (not major teaching)"] <- 11
ICU_c$ICUTypeID[ICU_c$ICUType == "Neonatal ICU"] <- 12
ICU_c$ICUTypeID[ICU_c$ICUType == "Neurologic ICU"] <- 13
ICU_c$ICUTypeID[ICU_c$ICUType == "Neurosurgical ICU"] <- 14
ICU_c$ICUTypeID[ICU_c$ICUType == "Pediatric ICU"] <- 15
ICU_c$ICUTypeID[ICU_c$ICUType == "Pediatric Ward"] <- 16
ICU_c$ICUTypeID[ICU_c$ICUType == "Surgical ICU"] <- 17
ICU_c$ICUTypeID[ICU_c$ICUType == "Surgical Ward"] <- 18
ICU_c$ICUTypeID[ICU_c$ICUType == "Trauma ICU"] <- 19
table(ICU_c$ICUTypeID, ICU_c$ICUType)

#Make grouping variables to try to understand these variations
#I should have done this with the IDs I created which would have been easier
#But I tried to get out of making ICUTypeID first - didn't succeed
#because I needed it for other programming - ICU vs Ward flag

Medvec <- c("Medical ICU (major teaching)", "Medical ICU (not major teaching)",
			"Medical Ward (major teaching)", "Medical Ward (not major teaching)")
MedSurgvec <- c("Medical/Surgical ICU (major teaching)", "Medical/Surgical ICU (not major teaching)",
		"Medical/Surgical Ward (major teaching)", "Medical/Surgical Ward (not major teaching)")
Surgvec <-c("Surgical ICU", "Surgical Ward")
Cardiacvec <- c("Cardiac ICU", "Cardiothoracic ICU")
Pedvec <- c("Pediatric ICU", "Pediatric Ward")
Specvec <- c("Burn ICU", "Neonatal ICU", "Neurologic ICU", "Neurosurgical ICU", "Trauma ICU")

ICU_c$ICUGrp <- 9
ICU_c$ICUGrp[ICU_c$ICUType %in% Medvec] <- 1
ICU_c$ICUGrp[ICU_c$ICUType %in% MedSurgvec] <- 2
ICU_c$ICUGrp[ICU_c$ICUType %in% Surgvec] <- 3
ICU_c$ICUGrp[ICU_c$ICUType %in% Cardiacvec] <- 4
ICU_c$ICUGrp[ICU_c$ICUType %in% Pedvec] <- 5
ICU_c$ICUGrp[ICU_c$ICUType %in% Specvec] <- 6
table(ICU_c$ICUType, ICU_c$ICUGrp)

#these are the ICUTypeIDs mean ICUs)
ICUcodes <- c(1, 2, 3, 4, 5, 8, 9, 12, 13, 14, 15, 17, 19)
#these are the ICUTypeIDs for the Wards
Wardcodes <- c(6, 7, 10, 11, 16, 18)

ICU_c$ICUStatus <- 9
ICU_c$ICUStatus[ICU_c$ICUTypeID %in% ICUcodes] <- 1
ICU_c$ICUStatus[ICU_c$ICUTypeID %in% Wardcodes] <- 2
table(ICU_c$ICUType, ICU_c$ICUStatus)

#Wardflag for regression
ICU_c$WardFlag <- 0
ICU_c$WardFlag[ICU_c$ICUStatus == 2] <- 1
table(ICU_c$WardFlag, ICU_c$ICUStatus)

#add flags for ICU types
ICU_c$MedSurgFlag <- 0
ICU_c$MedSurgFlag[ICU_c$ICUGrp == 2] <- 1
table(ICU_c$MedSurgFlag, ICU_c$ICUGrp)

ICU_c$SurgFlag <- 0
ICU_c$SurgFlag[ICU_c$ICUGrp == 3] <- 1
table(ICU_c$SurgFlag, ICU_c$ICUGrp)

ICU_c$Cardiac <- 0
ICU_c$Cardiac[ICU_c$ICUGrp == 4] <- 1
table(ICU_c$Cardiac, ICU_c$ICUGrp)

ICU_c$CardiacFlag <- 0
ICU_c$CardiacFlag[ICU_c$ICUGrp == 4] <- 1
table(ICU_c$CardiacFlag, ICU_c$ICUGrp)

ICU_c$PedFlag <- 0
ICU_c$PedFlag[ICU_c$ICUGrp == 5] <- 1
table(ICU_c$PedFlag, ICU_c$ICUGrp)

ICU_c$SpecFlag <- 0
ICU_c$SpecFlag[ICU_c$ICUGrp ==  6] <- 1
table(ICU_c$SpecFlag, ICU_c$ICUGrp)

#export for now
saveRDS(ICU_c, "ICU_c.rds")



