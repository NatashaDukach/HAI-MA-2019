#Read in ICU table
ICU <- readRDS("ICU_d.rds")

#all ICUs in this dataset have both rates
#so number of rows is the denominator
#for percentages this analysis

NumICUs <- nrow(ICU)

#create base table to populate
#make vectors for each of the columns

CatLev <- c("All", "T-Med", "T-MedSurg", "T-Surg", "T-Card",
		"T-Peds", "T-Spec", "S-ICU", "S-Ward",
		"St-Teach", "St-NonTeach")

TotalICUs <- rep(0, length(CatLev))
PropICUs <- rep(0, length(CatLev))
CLABSI_MeanRate <- rep(0, length(CatLev))
CLABSI_LL <- rep(0, length(CatLev))
CLABSI_UL <- rep(0, length(CatLev))
CAUTI_MeanRate <- rep(0, length(CatLev))
CAUTI_LL <- rep(0, length(CatLev))
CAUTI_UL <- rep(0, length(CatLev))

#combine columns into the data frame called ICU_tbl
ICU_tbl <- data.frame(CatLev, TotalICUs, PropICUs, 
	CLABSI_MeanRate, CLABSI_LL, CLABSI_UL,
	CAUTI_MeanRate, CAUTI_LL, CAUTI_UL)

#Fill in the Total ICUs column
ICU_tbl[1,2] <- NumICUs
TypeTbl <- as.data.frame(table(ICU$ICUGrp))
ICU_tbl[2:7,2] <- TypeTbl[,2]
WardTbl <- as.data.frame(table(ICU$WardFlag))
ICU_tbl[8:9,2] <- WardTbl[,2]
TeachTbl <- as.data.frame(table(ICU$Teaching))
ICU_tbl[10:11,2] <- TeachTbl[,2]

#calculate prop ICUs for all
ICU_tbl$PropICUs <- ICU_tbl$TotalICUs/NumICUs

#fill in overall rates and CI
CLABSImean <- mean(ICU$CLABSIRate) * 1000
CLABSIsd <- sd(ICU$CLABSIRate) * 1000
CLABSIse <- CLABSIsd/sqrt(NumICUs)
CLABSIme <- CLABSIse * 1.96
CLABSILower <- CLABSImean - CLABSIme
CLABSIUpper <- CLABSImean + CLABSIme

ICU_tbl[1,4] <- CLABSImean
ICU_tbl[1,5] <- CLABSILower
ICU_tbl[1,6] <- CLABSIUpper

CAUTImean <- mean(ICU$CAUTIRate) * 1000
CAUTIsd <- sd(ICU$CAUTIRate) * 1000
CAUTIse <- CAUTIsd/sqrt(NumICUs)
CAUTIme <- CAUTIse * 1.96
CAUTILower <- CAUTImean - CAUTIme
CAUTIUpper <- CAUTImean + CAUTIme

ICU_tbl[1,7] <- CAUTImean
ICU_tbl[1,8] <- CAUTILower
ICU_tbl[1,9] <- CAUTIUpper

#Fill in CLABSI by ICU type
#Remember: TypeTbl has n's by ICUGrp
names(TypeTbl) <- c("ICUGrp", "NumICUs")
CLABSImean_Type <-aggregate(as.numeric(ICU$CLABSIRate), by=list(ICU$ICUGrp), 
	FUN=mean, na.rm=TRUE)
names(CLABSImean_Type) <- c("ICUGrp", "CLABSImean")
CLABSImean_Type$CLABSImean1k <- CLABSImean_Type$CLABSImean * 1000
CLABSIsd_Type <-aggregate(as.numeric(ICU$CLABSIRate), by=list(ICU$ICUGrp), 
	FUN=sd, na.rm=TRUE)
names(CLABSIsd_Type) <- c("ICUGrp", "CLABSIsd")
CLABSIsd_Type$CLABSIsd1k <- CLABSIsd_Type$CLABSIsd * 1000

#put the datasets together
merge1 <- merge(TypeTbl, CLABSImean_Type, by = "ICUGrp")
merge2 <- merge(merge1, CLABSIsd_Type, by = "ICUGrp")
merge2$CLABSIse <- merge2$CLABSIsd1k/sqrt(merge2$NumICUs)
merge2$CLABSIme <- merge2$CLABSIse * 1.96
merge2$CLABSILower <- merge2$CLABSImean1k - merge2$CLABSIme
merge2$CLABSIUpper <- merge2$CLABSImean1k + merge2$CLABSIme

#add info to ICU_Tbl
ICU_tbl[2:7, 4] <- merge2$CLABSImean1k
ICU_tbl[2:7, 5] <- merge2$CLABSILower
ICU_tbl[2:7, 6] <- merge2$CLABSIUpper

#fill in CAUTI by ICU Type
#Remember: TypeTbl has n's by ICUGrp
CAUTImean_Type <-aggregate(as.numeric(ICU$CAUTIRate), by=list(ICU$ICUGrp), 
	FUN=mean, na.rm=TRUE)
names(CAUTImean_Type) <- c("ICUGrp", "CAUTImean")
CAUTImean_Type$CAUTImean1k <- CAUTImean_Type$CAUTImean * 1000
CAUTIsd_Type <-aggregate(as.numeric(ICU$CAUTIRate), by=list(ICU$ICUGrp), 
	FUN=sd, na.rm=TRUE)
names(CAUTIsd_Type) <- c("ICUGrp", "CAUTIsd")
CAUTIsd_Type$CAUTIsd1k <- CAUTIsd_Type$CAUTIsd * 1000

#put the datasets together
merge1 <- merge(TypeTbl, CAUTImean_Type, by = "ICUGrp")
merge2 <- merge(merge1, CAUTIsd_Type, by = "ICUGrp")
merge2$CAUTIse <- merge2$CAUTIsd1k/sqrt(merge2$NumICUs)
merge2$CAUTIme <- merge2$CAUTIse * 1.96
merge2$CAUTILower <- merge2$CAUTImean1k - merge2$CAUTIme
merge2$CAUTIUpper <- merge2$CAUTImean1k + merge2$CAUTIme

#add info to ICU_Tbl
ICU_tbl[2:7, 7] <- merge2$CAUTImean1k
ICU_tbl[2:7, 8] <- merge2$CAUTILower
ICU_tbl[2:7, 9] <- merge2$CAUTIUpper

#Fill in CLABSI by Setting (Ward)
#Remember: WardTbl has n's by Setting
names(WardTbl) <- c("WardFlag", "NumICUs")
CLABSImean_Set <-aggregate(as.numeric(ICU$CLABSIRate), by=list(ICU$WardFlag), 
	FUN=mean, na.rm=TRUE)
names(CLABSImean_Set) <- c("WardFlag", "CLABSImean")
CLABSImean_Set$CLABSImean1k <- CLABSImean_Set$CLABSImean * 1000
CLABSIsd_Set <-aggregate(as.numeric(ICU$CLABSIRate), by=list(ICU$WardFlag), 
	FUN=sd, na.rm=TRUE)
names(CLABSIsd_Set) <- c("WardFlag", "CLABSIsd")
CLABSIsd_Set$CLABSIsd1k <- CLABSIsd_Set$CLABSIsd * 1000

#put the datasets together
merge1 <- merge(WardTbl, CLABSImean_Set, by = "WardFlag")
merge2 <- merge(merge1, CLABSIsd_Set, by = "WardFlag")
merge2$CLABSIse <- merge2$CLABSIsd1k/sqrt(merge2$NumICUs)
merge2$CLABSIme <- merge2$CLABSIse * 1.96
merge2$CLABSILower <- merge2$CLABSImean1k - merge2$CLABSIme
merge2$CLABSIUpper <- merge2$CLABSImean1k + merge2$CLABSIme

#add info to ICU_Tbl
ICU_tbl[8:9, 4] <- merge2$CLABSImean1k
ICU_tbl[8:9, 5] <- merge2$CLABSILower
ICU_tbl[8:9, 6] <- merge2$CLABSIUpper

#Fill in CAUTI by Setting (Ward)
#Remember: WardTbl has n's by Setting
CAUTImean_Set <-aggregate(as.numeric(ICU$CAUTIRate), by=list(ICU$WardFlag), 
	FUN=mean, na.rm=TRUE)
names(CAUTImean_Set) <- c("WardFlag", "CAUTImean")
CAUTImean_Set$CAUTImean1k <- CAUTImean_Set$CAUTImean * 1000
CAUTIsd_Set <-aggregate(as.numeric(ICU$CAUTIRate), by=list(ICU$WardFlag), 
	FUN=sd, na.rm=TRUE)
names(CAUTIsd_Set) <- c("WardFlag", "CAUTIsd")
CAUTIsd_Set$CAUTIsd1k <- CAUTIsd_Set$CAUTIsd * 1000

#put the datasets together
merge1 <- merge(WardTbl, CAUTImean_Set, by = "WardFlag")
merge2 <- merge(merge1, CAUTIsd_Set, by = "WardFlag")
merge2$CAUTIse <- merge2$CAUTIsd1k/sqrt(merge2$NumICUs)
merge2$CAUTIme <- merge2$CAUTIse * 1.96
merge2$CAUTILower <- merge2$CAUTImean1k - merge2$CAUTIme
merge2$CAUTIUpper <- merge2$CAUTImean1k + merge2$CAUTIme

#add info to ICU_Tbl
ICU_tbl[8:9, 7] <- merge2$CAUTImean1k
ICU_tbl[8:9, 8] <- merge2$CAUTILower
ICU_tbl[8:9, 9] <- merge2$CAUTIUpper

#Fill in CLABSI by Teaching Status
#Remember: TeachTbl has n's by Setting
names(TeachTbl) <- c("Teaching", "NumICUs")
CLABSImean_Teach <-aggregate(as.numeric(ICU$CLABSIRate), by=list(ICU$Teaching), 
	FUN=mean, na.rm=TRUE)
names(CLABSImean_Teach) <- c("Teaching", "CLABSImean")
CLABSImean_Teach$CLABSImean1k <- CLABSImean_Teach$CLABSImean * 1000
CLABSIsd_Teach <-aggregate(as.numeric(ICU$CLABSIRate), by=list(ICU$Teaching), 
	FUN=sd, na.rm=TRUE)
names(CLABSIsd_Teach) <- c("Teaching", "CLABSIsd")
CLABSIsd_Teach$CLABSIsd1k <- CLABSIsd_Teach$CLABSIsd * 1000

#put the datasets together
merge1 <- merge(TeachTbl, CLABSImean_Teach, by = "Teaching")
merge2 <- merge(merge1, CLABSIsd_Teach, by = "Teaching")
merge2$CLABSIse <- merge2$CLABSIsd1k/sqrt(merge2$NumICUs)
merge2$CLABSIme <- merge2$CLABSIse * 1.96
merge2$CLABSILower <- merge2$CLABSImean1k - merge2$CLABSIme
merge2$CLABSIUpper <- merge2$CLABSImean1k + merge2$CLABSIme

#add info to ICU_Tbl
ICU_tbl[10:11, 4] <- merge2$CLABSImean1k
ICU_tbl[10:11, 5] <- merge2$CLABSILower
ICU_tbl[10:11, 6] <- merge2$CLABSIUpper

#Fill in CAUTI by Teaching Status
#Remember: TeachTbl has n's by Setting
CAUTImean_Teach <-aggregate(as.numeric(ICU$CAUTIRate), by=list(ICU$Teaching), 
	FUN=mean, na.rm=TRUE)
names(CAUTImean_Teach) <- c("Teaching", "CAUTImean")
CAUTImean_Teach$CAUTImean1k <- CAUTImean_Teach$CAUTImean * 1000
CAUTIsd_Teach <-aggregate(as.numeric(ICU$CAUTIRate), by=list(ICU$Teaching), 
	FUN=sd, na.rm=TRUE)
names(CAUTIsd_Teach) <- c("Teaching", "CAUTIsd")
CAUTIsd_Teach$CAUTIsd1k <- CAUTIsd_Teach$CAUTIsd * 1000

#put the datasets together
merge1 <- merge(TeachTbl, CAUTImean_Teach, by = "Teaching")
merge2 <- merge(merge1, CAUTIsd_Teach, by = "Teaching")
merge2$CAUTIse <- merge2$CAUTIsd1k/sqrt(merge2$NumICUs)
merge2$CAUTIme <- merge2$CAUTIse * 1.96
merge2$CAUTILower <- merge2$CAUTImean1k - merge2$CAUTIme
merge2$CAUTIUpper <- merge2$CAUTImean1k + merge2$CAUTIme

#add info to ICU_Tbl
ICU_tbl[10:11, 7] <- merge2$CAUTImean1k
ICU_tbl[10:11, 8] <- merge2$CAUTILower
ICU_tbl[10:11, 9] <- merge2$CAUTIUpper

#export
write.csv(ICU_tbl, "ICU_tbl.csv")

#tests
CLABSIType_aov <- aov(CLABSIRate ~ ICUGrp, data = ICU)
summary(CLABSIType_aov)
CAUTIType_aov <- aov(CAUTIRate ~ ICUGrp, data = ICU)
summary(CAUTIType_aov)

#separate datasets to do ttests
nrow(ICU)
Ward <- subset(ICU, WardFlag == 1)
nrow(Ward)
ICU_subset <- subset(ICU, WardFlag == 0)
nrow(ICU_subset)

t.test(Ward$CLABSIRate, ICU_subset$CLABSIRate)
t.test(Ward$CAUTIRate, ICU_subset$CAUTIRate)

nrow(ICU)
Teaching <- subset(ICU, Teaching == 1)
nrow(Teaching)
NoTeach <- subset(ICU, Teaching == 2)
nrow(NoTeach)

t.test(Teaching$CLABSIRate, NoTeach$CLABSIRate)
t.test(Teaching$CAUTIRate, NoTeach$CAUTIRate)

