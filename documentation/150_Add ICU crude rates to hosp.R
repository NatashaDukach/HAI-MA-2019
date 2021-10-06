#patch on ICU variables to hospital table
#read in current versions of tables
hosp_c <- readRDS("hosp_c.rds")
ICU_d <- readRDS("ICU_d.rds")
colnames(ICU_d)
ICU_d$CAUTIDays

#assemble datasets to patch on using aggregate
CLABSIct <-aggregate(as.numeric(ICU_d$CLABSINum), by=list(ICU_d$OrgID), FUN=sum, na.rm=TRUE)
names(CLABSIct) <- c("OrgID", "CLABSICases")
CLABSIdays <-aggregate(as.numeric(ICU_d$CLABSIDays), by=list(ICU_d$OrgID), FUN=sum, na.rm=TRUE)
names(CLABSIdays) <- c("OrgID", "CLABSICathdays")
CAUTIct <-aggregate(as.numeric(ICU_d$CAUTINum), by=list(ICU_d$OrgID), FUN=sum, na.rm=TRUE)
names(CAUTIct) <- c("OrgID", "CAUTICases")
CAUTIdays <-aggregate(as.numeric(ICU_d$CAUTIDays), by=list(ICU_d$OrgID), FUN=sum, na.rm=TRUE)
names(CAUTIdays) <- c("OrgID", "CAUTICathdays")

#put datasets together
nrow(CLABSIct)
nrow(CLABSIdays)
add1 <- merge(CLABSIct, CLABSIdays, by="OrgID")
nrow(add1)
add2 <- merge(add1, CAUTIct, by="OrgID")
nrow(add2)
add3 <- merge(add2, CAUTIdays, by="OrgID")
nrow(add3)
colnames(add3)

#Add crude rates
add3$CLABSIRateCrude <- add3$CLABSICases/add3$CLABSICathdays
add3$CAUTIRateCrude <- add3$CAUTICases/add3$CAUTICathdays

#now left join add3 onto hosp_c
nrow(hosp_c)
hosp_d <- merge(x = hosp_c, y = add3, by = "OrgID", all.x = TRUE)
nrow(hosp_d)

#now make some more variables
hosp_d$AllCases <- hosp_d$CAUTICases + hosp_d$CLABSICases
hosp_d$AllCathdays <- hosp_d$CAUTICathdays + hosp_d$CLABSICathdays
hosp_d$CombinedRateCrude <- hosp_d$AllCases/hosp_d$AllCathdays

#export
saveRDS(hosp_d, "hosp_d.rds")




