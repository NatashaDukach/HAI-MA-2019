#add TPS
#read in hosp_a
hosp_a <- readRDS("hosp_a.rds")
colnames(hosp_a)[1] <- "OrgID"
#read in TPS dataset
TPS_a <- read.csv("TPS.csv", header=TRUE, sep=",")
nrow(TPS_a)

#patch TPS_a onto hosp_a to make hosp_b
nrow(TPS_a)
nrow(hosp_a)
hosp_b <- merge(x = hosp_a, y = TPS_a, by = "OrgID", all.x = TRUE)
nrow(hosp_b)

#figure out quartiles
quant_tps <- as.data.frame(quantile(hosp_b$HospPerfScore, na.rm = TRUE))
#label percentiles
TPSQ3 <- quant_tps[4,1]
TPSQ2 <- quant_tps[3,1]
TPSQ1 <- quant_tps[2,1]

hosp_b$PerfScoreCat <- 9
hosp_b$PerfScoreCat[hosp_b$HospPerfScore > TPSQ3] <- 4
hosp_b$PerfScoreCat[hosp_b$HospPerfScore <= TPSQ3 & hosp_b$HospPerfScore > TPSQ2] <- 3
hosp_b$PerfScoreCat[hosp_b$HospPerfScore <= TPSQ2 & hosp_b$HospPerfScore > TPSQ1] <- 2
hosp_b$PerfScoreCat[hosp_b$HospPerfScore <= TPSQ1] <- 1
table(hosp_b$PerfScoreCat, hosp_b$HospPerfScore, useNA = c("always"))
hosp_b$TPSQ1 <- 0
hosp_b$TPSQ1[hosp_b$PerfScoreCat == 1] <- 1
table(hosp_b$PerfScoreCat, hosp_b$TPSQ1)

#fill in county names by hand where there is no TPS score so the
#HospCountyName did not get filled in
suffolk_hosps <- c(12698, 12705, 12709, 13098, 30172) 
worcester_hosps <- c(11303, 12700, 13262) 
plymouth_hosps <- c(12574) 
bristol_hosps <- c(12576) 
middlesex_hosps <- c(12714, 13215, 13309) 
berkshire_hosps <- c(13189)
essex_hosps <- c(13301, 14175, 14360, 57659)
dukes_hosps <- c(12833)
nantucket_hosps <- c(13206)

#first copy this to a character field because it won't
#let me add levels of this county factor
#and Dukes and Nantucket only have one

hosp_b$HospCountyName_c <- as.character(hosp_b$HospCountyName)

hosp_b$HospCountyName_c[hosp_b$OrgID %in% suffolk_hosps] <- "Suffolk"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% worcester_hosps] <- "Worcester"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% plymouth_hosps] <- "Plymouth"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% bristol_hosps] <- "Bristol"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% middlesex_hosps] <- "Middlesex"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% berkshire_hosps] <- "Berkshire"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% essex_hosps] <- "Essex"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% dukes_hosps] <- "Dukes"
hosp_b$HospCountyName_c[hosp_b$OrgID %in% nantucket_hosps] <- "Nantucket"

table(hosp_b$HospCountyName_c, useNA = c("always"))

#now back populate it into HospCountyName as a factor
hosp_b$HospCountyName <- as.factor(hosp_b$HospCountyName_c)

table(hosp_b$HospCountyName, useNA = c("always"))

#read out hosp_b
saveRDS(hosp_b, "hosp_b.rds")

