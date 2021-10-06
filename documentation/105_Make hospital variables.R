#Make hospital variables
#Make HospTypeID

hosp_a <- readRDS("hosp_a.rds")
colnames(hosp_a)

#teaching grouping variables
hosp_a$HospTypeID <- 9
hosp_a$HospTypeID[hosp_a$H9HospType == "Nonteaching"] <- 1
hosp_a$HospTypeID[hosp_a$H9HospType == "Undergraduate teaching"] <- 2
hosp_a$HospTypeID[hosp_a$H9HospType == "Graduate teaching"] <- 3
hosp_a$HospTypeID[hosp_a$H9HospType == "Major teaching"] <- 4
table(hosp_a$HospTypeID, hosp_a$H9HospType)
#this is for the descriptive table
hosp_a$Teaching <- 2
hosp_a$Teaching[hosp_a$HospTypeID %in% 2:4] <- 1
table(hosp_a$HospTypeID, hosp_a$Teaching)
#This is a flag for the regession - so the slope is 0 to 1
hosp_a$TeachFlag <-0
hosp_a$TeachFlag[hosp_a$Teaching == 1] <- 1
table(hosp_a$TeachFlag, hosp_a$Teaching)

#Profit
hosp_a$ProfitID <- 9
hosp_a$ProfitID[hosp_a$H9ProfitStatus== "For-profit"] <- 1
hosp_a$ProfitID[hosp_a$H9ProfitStatus== "Not-for-profit"] <- 2
table(hosp_a$ProfitID, hosp_a$H9ProfitStatus)
#flag for the regression
hosp_a$ProfitFlag <- 0
hosp_a$ProfitFlag[hosp_a$ProfitID == 1] <- 1
table(hosp_a$ProfitID, hosp_a$ProfitFlag)

#Hosp bed categories
hosp_a$BedCat <- 9
hosp_a$BedCat[hosp_a$H9Beds < 200] <- 1
hosp_a$BedCat[hosp_a$H9Beds >= 200 & hosp_a$H9Beds < 400] <- 2
hosp_a$BedCat[hosp_a$H9Beds >= 400] <- 3
table(hosp_a$BedCat, hosp_a$H9Beds)
hosp_a$Bed2 <- 0
hosp_a$Bed2[hosp_a$BedCat ==2] <- 1
table(hosp_a$BedCat, hosp_a$Bed2)
hosp_a$Bed3 <- 0
hosp_a$Bed3[hosp_a$BedCat ==3] <- 1
table(hosp_a$BedCat, hosp_a$Bed3)

#ICU bed categories
hosp_a$ICUBedCat <- 9
hosp_a$ICUBedCat[hosp_a$H9ICUBeds == 0] <- 1
hosp_a$ICUBedCat[hosp_a$H9ICUBeds>=1 & hosp_a$H9ICUBeds<=9] <- 2
hosp_a$ICUBedCat[hosp_a$H9ICUBeds>9 & hosp_a$H9ICUBeds<=49] <- 3
hosp_a$ICUBedCat[hosp_a$H9ICUBeds>= 50] <- 4
table(hosp_a$ICUBedCat, hosp_a$H9ICUBeds)
hosp_a$ICUBed3 <- 0
hosp_a$ICUBed3[hosp_a$ICUBedCat == 3] <- 1
table(hosp_a$ICUBedCat, hosp_a$ICUBed3)
hosp_a$ICUBed4 <- 0
hosp_a$ICUBed4[hosp_a$ICUBedCat == 4] <- 1
table(hosp_a$ICUBedCat, hosp_a$ICUBed4)

#admissions
hosp_a$AdmCat <- 9
hosp_a$AdmCat[hosp_a$H9NumAdm <10000] <- 1
hosp_a$AdmCat[hosp_a$H9NumAdm >=10000 & hosp_a$H9NumAdm <20000] <- 2
hosp_a$AdmCat[hosp_a$H9NumAdm >=20000] <- 3
table(hosp_a$AdmCat, hosp_a$H9NumAdm)
hosp_a$Adm2 <- 0
hosp_a$Adm2[hosp_a$AdmCat == 2] <- 1
table(hosp_a$AdmCat, hosp_a$Adm2)
hosp_a$Adm3 <- 0
hosp_a$Adm3[hosp_a$AdmCat == 3] <- 1
table(hosp_a$AdmCat, hosp_a$Adm3)

#admissions
hosp_a$PatDayCat <- 9
hosp_a$PatDayCat[hosp_a$H9NumPatDays <50000] <- 1
hosp_a$PatDayCat[hosp_a$H9NumPatDays >=50000 & hosp_a$H9NumPatDays <100000] <- 2
hosp_a$PatDayCat[hosp_a$H9NumPatDays >=100000] <- 3
table(hosp_a$PatDayCat, hosp_a$H9NumPatDays)
hosp_a$PatDay2 <- 0
hosp_a$PatDay2[hosp_a$PatDayCat == 2] <- 1
table(hosp_a$PatDayCat, hosp_a$PatDay2)
hosp_a$PatDay3 <- 0
hosp_a$PatDay3[hosp_a$PatDayCat == 3] <- 1
table(hosp_a$PatDayCat, hosp_a$PatDay3)

#affiliation
table(hosp_a$H9Affiliation, useNA = c("always"))
hosp_a$MedAfilGrp <- 9
hosp_a$MedAfilGrp[hosp_a$H9Affiliation == "Yes"] <- 1
hosp_a$MedAfilGrp[hosp_a$H9Affiliation == "No"] <- 2
table(hosp_a$MedAfilGrp, hosp_a$H9Affiliation)
hosp_a$MedAfilFlag <- 0
hosp_a$MedAfilFlag[hosp_a$MedAfilGrp == 1] <- 1
table(hosp_a$MedAfilGrp, hosp_a$MedAfilFlag)

#read out hosp_a
saveRDS(hosp_a, "hosp_a.rds")



