#Make hospital results table
#read in analytic to hosp
hosp <- readRDS("hosp_e.rds")

#create base table to populate
#make vectors for each of the four columns
CatLev <- c("All", "T-Nonteach", "T-Undergrad", "T-Grad", "T-Major",
		"P-ForProfit", "P-NonProfit", "S-Bed1", "S-Bed2", "S-Bed3",
		"I-NoBed", "I-Bed1", "I-Bed2", "I-Bed3", 
		"A-Adm1", "A-Adm2", "A-Adm3", "PDay1", "PDay2", "PDay3")

All <- rep(0, length(CatLev))
Teaching <- rep(0, length(CatLev))
NonTeaching <- rep(0, length(CatLev))

#combine 4 columns into the data frame called hosp_tbl
hosp_tbl <- data.frame(CatLev, All, Teaching, NonTeaching)

#replace 0's in table
#Top row of table
TotalSample <- nrow(hosp)
TeachFreq <- as.data.frame(table(hosp$Teaching))

hosp_tbl[1,2] <- TotalSample
hosp_tbl[1,3:4] <- TeachFreq[,2]

#Hospital Type
TypeFreq <- as.data.frame(table(hosp$HospTypeID))
hosp_tbl[2:5, 2] <- TypeFreq[,2]
TypeTeachFreq <- as.data.frame(table(hosp$HospTypeID, hosp$Teaching))
hosp_tbl[2:5, 3] <- TypeTeachFreq[1:4,3]
hosp_tbl[2:5, 4] <- TypeTeachFreq[5:8,3]

#Profit Status
ProfFreq <- as.data.frame(table(hosp$ProfitID))
hosp_tbl[6:7, 2] <- ProfFreq[,2]
ProfTeachFreq <- as.data.frame(table(hosp$ProfitID, hosp$Teaching))
hosp_tbl[6:7, 3] <- ProfTeachFreq[1:2,3]
hosp_tbl[6:7, 4] <- ProfTeachFreq[3:4,3]

#Beds
BedFreq <- as.data.frame(table(hosp$BedCat))
hosp_tbl[8:10, 2] <- BedFreq[,2]
BedTeachFreq <- as.data.frame(table(hosp$BedCat, hosp$Teaching))
hosp_tbl[8:10, 3] <- BedTeachFreq[1:3,3]
hosp_tbl[8:10, 4] <- BedTeachFreq[4:6,3]

#ICU Beds
ICUBedFreq <- as.data.frame(table(hosp$ICUBedCat))
hosp_tbl[11:14, 2] <- ICUBedFreq[,2]
ICUBedTeachFreq <- as.data.frame(table(hosp$ICUBedCat, hosp$Teaching))
hosp_tbl[11:14, 3] <- ICUBedTeachFreq[1:4,3]
hosp_tbl[11:14, 4] <- ICUBedTeachFreq[5:8,3]

#Admission
AdmCatFreq <- as.data.frame(table(hosp$AdmCat))
hosp_tbl[15:17, 2] <- AdmCatFreq[,2]
AdmCatTeachFreq <- as.data.frame(table(hosp$AdmCat, hosp$Teaching))
hosp_tbl[15:17, 3] <- AdmCatTeachFreq[1:3,3]
hosp_tbl[15:17, 4] <- AdmCatTeachFreq[4:6,3]

#Patdays
PatDayFreq <- as.data.frame(table(hosp$PatDayCat))
hosp_tbl[18:20, 2] <- PatDayFreq[,2]
PatDayTeachFreq <- as.data.frame(table(hosp$PatDayCat, hosp$Teaching))
hosp_tbl[18:20, 3] <- PatDayTeachFreq[1:3,3]
hosp_tbl[18:20, 4] <- PatDayTeachFreq[4:6,3]

#export table
write.csv(hosp_tbl, "hosp_tbl.csv")





