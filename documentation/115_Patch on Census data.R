#add census
#read in hosp_b
hosp_b <- readRDS("hosp_b.rds")
colnames(hosp_b)
class(hosp_b$HospCountyName)
#read in county data
county_a <- read.csv("CountyData.csv", header = TRUE, sep = ",")
colnames(county_a)
class(county_a$HospCountyName)

#left join on HospCountyName
nrow(hosp_b)
hosp_c <- merge(x = hosp_b, y = county_a, by = "HospCountyName", all.x = TRUE)
nrow(hosp_c)
table(hosp_c$HospCountyName, useNA = c("always"))

#add PropNonwhite
hosp_c$PropNonwhite <- 1.0 - hosp_c$PropWhite

#export hosp_c
saveRDS(hosp_c, "hosp_c.rds")
