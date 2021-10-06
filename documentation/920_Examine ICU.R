ICU_c$NoCLABSIDays <- 0
ICU_c$NoCLABSIDays[is.na(ICU_c$CLABSIDays)] <- 1
table(ICU_c$CLABSIDays, ICU_c$NoCLABSIDays, useNA = c("always"))
table(ICU_c$NoCLABSIDays)
#they are all filled in, but a few = 0 - why?

ZeroCLABSI <- subset(ICU_c, CLABSIDays == 0)
#these are 5 Pediatric Wards, 1 Trauma ICU, and 1 Surgical Ward.

table(ICU_c$ICUType, is.na(ICU_c$CLABSIDays), useNA = c("always"))

#trying to identify ICU's who won't participate in this analysis
as.data.frame(table(ICU_c$NoCAUTIDays, ICU_c$NoCLABSIDays))
#let's look at those with no CLABSI or CAUTI days
NoCatheters <- subset(ICU_c, NoCAUTIDays == 1 & NoCLABSIDays == 1)
#these are 4 pediatric wards (normal) 
#and the Trauma ICU - which sounds fishy. But I looked up the OrgID
#and this is Nantucket so by luck they did not have any - not because
#they don't do it.
